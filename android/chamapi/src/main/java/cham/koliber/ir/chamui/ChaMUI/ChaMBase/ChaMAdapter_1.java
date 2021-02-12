package cham.koliber.ir.chamui.ChaMUI.ChaMBase;

import android.content.Context;
import android.support.v7.util.SortedList;
import android.support.v7.widget.DefaultItemAnimator;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Filter;
import android.widget.Filterable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.concurrent.ConcurrentSkipListMap;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMRecyclerView;
import cham.koliber.ir.chamui.R;

public abstract class ChaMAdapter<ChaMAdapterHolder extends RecyclerView.ViewHolder,ChaMAdapterValue> extends RecyclerView.Adapter<ChaMAdapterHolder> implements Filterable{

    private ChaMActivity activity;
    private ChaMRecyclerView recycler_view;
    private ChaMAdapterSearcher<ChaMAdapterValue> searcher;
    private int layout_id;
    private boolean loader;
    private boolean loading_up;
    private boolean loading_down;
    private String first_key;
    private String last_key;

    private ConcurrentSkipListMap<String,ChaMAdapterItem<ChaMAdapterValue>> items;
    private SortedList<ChaMAdapterItem<ChaMAdapterValue>> filters;
    private RecyclerView.OnScrollListener scroll_listener;

    public ChaMAdapter(ChaMActivity activity, final ChaMRecyclerView recycler_view, ChaMAdapterSearcher<ChaMAdapterValue> searcher, int layout_id) {
        this.activity = activity;
        this.recycler_view = recycler_view;
        this.searcher = searcher;
        this.layout_id = layout_id;
        this.loader = false;
        this.loading_up = false;
        this.loading_down = false;
        this.first_key = ChaMCoreAPI.Time.current();
        this.last_key =  ChaMCoreAPI.Time.current();
        this.items = new ConcurrentSkipListMap<>();
        Class klass = ChaMAdapterItem.class;
        this.filters = new SortedList<>(klass, new SortedList.Callback<ChaMAdapterItem<ChaMAdapterValue>>() {
            @Override
            public int compare(ChaMAdapterItem<ChaMAdapterValue> o1, ChaMAdapterItem<ChaMAdapterValue> o2) {
                return o1.getKey().compareTo(o2.getKey());
            }

            @Override
            public boolean areContentsTheSame(ChaMAdapterItem<ChaMAdapterValue> oldItem, ChaMAdapterItem<ChaMAdapterValue> newItem) {
                return oldItem.hashCode() == newItem.hashCode();
            }

            @Override
            public boolean areItemsTheSame(ChaMAdapterItem<ChaMAdapterValue> item1, ChaMAdapterItem<ChaMAdapterValue> item2) {
                return item1.getKey().equals(item2.getKey());
            }

            @Override
            public void onInserted(final int position, final int count) {
                getChaMActivity().runOnUiThread(new Runnable() {
                    @Override
                    public void run() {
                        notifyItemRangeInserted(position, count);
                    }
                });
            }

            @Override
            public void onRemoved(final int position, final int count) {
                getChaMActivity().runOnUiThread(new Runnable() {
                    @Override
                    public void run() {
                        notifyItemRangeRemoved(position, count);
//                        notifyItemRangeChanged(position,getItemCount());
                    }
                });
            }

            @Override
            public void onChanged(final int position, final int count) {
                getChaMActivity().runOnUiThread(new Runnable() {
                    @Override
                    public void run() {
                        notifyItemRangeChanged(position, count);
                    }
                });
            }

            @Override
            public void onMoved(final int fromPosition, final int toPosition) {
                getChaMActivity().runOnUiThread(new Runnable() {
                    @Override
                    public void run() {
                        notifyItemMoved(fromPosition, toPosition);
                    }
                });
            }
        });
        this.scroll_listener = new RecyclerView.OnScrollListener() {
            private LinearLayoutManager layout_manager;
            @Override
            public void onScrolled(RecyclerView recyclerView, int dx, int dy) {
                super.onScrolled(recyclerView, dx, dy);
                if(layout_manager == null){
                    layout_manager = (LinearLayoutManager) recycler_view.getLayoutManager();
                    return;
                }
                if(getItemCount() > 0){
                    int current_first = layout_manager.findFirstVisibleItemPosition(); // down  -> 0
                    int current_last = layout_manager.findLastVisibleItemPosition();   // up    -> size
                    if(current_first <= 5 && !loading_down){
                        loading_up = true;
                        recycler_view.removeOnScrollListener(scroll_listener);
                        init_loader(ChaMAdapterLoad.CHAM_ADAPTER_LOAD_UP);
                    }
                    if(current_last >= getItemCount()-5 && !loading_up){
                        loading_down = true;
                        recycler_view.removeOnScrollListener(scroll_listener);
                        init_loader(ChaMAdapterLoad.CHAM_ADAPTER_LOAD_DOWN);
                    }
                }
            }
        };
        init_recycler();
    }

    private void init_recycler(){
        recycler_view.setLayoutManager(new ChaMAdapterLayoutManager(activity));
        recycler_view.setItemAnimator(new ChaMAdapterAnimator());
        recycler_view.setNestedScrollingEnabled(false);
        recycler_view.setHasFixedSize(true);
        recycler_view.setItemViewCacheSize(20);
        recycler_view.setDrawingCacheEnabled(true);
        recycler_view.setLayoutFrozen(false);
        recycler_view.setWillNotCacheDrawing(false);
        recycler_view.setDrawingCacheQuality(View.DRAWING_CACHE_QUALITY_HIGH);
        if(loader){
            recycler_view.addOnScrollListener(scroll_listener);
        }
    }

    private void init_loader(ChaMAdapterLoad load){
        if(load == ChaMAdapterLoad.CHAM_ADAPTER_LOAD_UP){
            this.first_key = ChaMCoreAPI.Time.previews(first_key);
            items.put(first_key,new ChaMAdapterItem<ChaMAdapterValue>(first_key,false,null));
        }else{
            this.last_key = ChaMCoreAPI.Time.next(last_key);
            items.put(last_key,new ChaMAdapterItem<ChaMAdapterValue>(last_key,false,null));
        }
        filter("");
        load_holder(load);
    }

    @Override
    public int getItemViewType(int position) {
        return filters.get(position).getValue() == null ? R.layout.cham_adapterprogress : layout_id;
    }

    @Override
    public ChaMAdapterHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        LayoutInflater inflater = LayoutInflater.from(activity);
        View view = inflater.inflate(viewType, parent, false);
        return create_holder(view);
    }

    @Override
    public void onBindViewHolder(ChaMAdapterHolder holder, int position){
        if(filters.get(position).getValue() != null){
            init_holder(holder,filters.get(position));
        }
    }

    @Override
    public int getItemCount() {
        return filters.size();
    }

    @Override
    public Filter getFilter(){
        return new Filter() {
            @Override
            protected FilterResults performFiltering(CharSequence constraint) {return null;}

            @Override
            protected void publishResults(CharSequence constraint, FilterResults results) {
                String search = constraint.toString();
                for(int cursor = 0 ; cursor < getItemCount() ; cursor++){
                    ChaMAdapterItem<ChaMAdapterValue> item = filters.get(cursor);
                    if((searcher != null && !searcher.search(search, item.getValue())) || item.getKey().compareTo(first_key) < 0 || item.getKey().compareTo(last_key) > 0 || items.get(item.getKey()) == null){
                        filters.removeItemAt(cursor);
                    }
                }
                filters.addAll(items.subMap(first_key,last_key).values());
            }
        };
    }

    protected ChaMActivity getChaMActivity(){
        return activity;
    }

    protected ChaMRecyclerView getChaMRecyclerView(){
        return recycler_view;
    }

    protected abstract ChaMAdapterHolder create_holder(View view);

    protected abstract void init_holder(ChaMAdapterHolder holder, ChaMAdapterItem<ChaMAdapterValue> item);

    protected abstract void load_holder(ChaMAdapterLoad load);

    public void loader(boolean loader){
        this.loader = loader;
        if(loader && getItemCount() <= 0){
            if(getItemCount() > 0){
                recycler_view.addOnScrollListener(scroll_listener);
            }else{
                loading_up = true;
                recycler_view.removeOnScrollListener(scroll_listener);
                init_loader(ChaMAdapterLoad.CHAM_ADAPTER_LOAD_UP);
            }
        }else if(!loader){
            recycler_view.removeOnScrollListener(scroll_listener);
        }
    }

    public void load(ChaMAdapterLoad load, ChaMAdapterValue value){
        String key;
        if(load == ChaMAdapterLoad.CHAM_ADAPTER_LOAD_UP){
            key = ChaMCoreAPI.Time.previews(items.firstKey() == null ? first_key : items.firstKey());
        }else{
            key = ChaMCoreAPI.Time.next(items.lastKey() == null ? last_key : items.lastKey());
        }
        items.put(key, new ChaMAdapterItem<>(key, false, value));
    }

    public void end(ChaMAdapterLoad load){
        if(load == ChaMAdapterLoad.CHAM_ADAPTER_LOAD_UP){
            items.remove(first_key);
            this.first_key = items.firstKey() == null ? first_key : items.firstKey();
        }else{
            items.remove(last_key);
            this.last_key = items.lastKey() == null ? last_key : items.lastKey();
        }
        filter("");
        if(loader){
            recycler_view.addOnScrollListener(scroll_listener);
        }
    }

    public void remove(String key){
        items.remove(key);
        filter("");
    }

    public void select(String key){
        for(int cursor = 0 ; cursor < getItemCount() ; cursor++){
            ChaMAdapterItem<ChaMAdapterValue> item = filters.get(cursor);
            if(item.getKey().equals(key)){
                item.setSelected(!item.getSelected());
                final int finalCursor = cursor;
                getChaMActivity().runOnUiThread(new Runnable() {
                    @Override
                    public void run() {
                        notifyItemChanged(finalCursor);
                    }
                });
                break;
            }
        }
    }

    public void change(String key){
        for(int cursor = 0 ; cursor < getItemCount() ; cursor++){
            ChaMAdapterItem<ChaMAdapterValue> item = filters.get(cursor);
            if(item.getKey().equals(key)){
                final int finalCursor = cursor;
                getChaMActivity().runOnUiThread(new Runnable() {
                    @Override
                    public void run() {
                        notifyItemChanged(finalCursor);
                    }
                });
                break;
            }
        }
    }

    public ChaMAdapterValue get(String key){
        return items.get(key).getValue();
    }

    public ChaMAdapterValue first(){
        return items.firstEntry().getValue().getValue();
    }

    public ChaMAdapterValue last(){
        return items.lastEntry().getValue().getValue();
    }

    public void filter(String search){
        getFilter().filter(search);
    }

    public ArrayList<ChaMAdapterValue> selection(){
        ArrayList<ChaMAdapterValue> result = new ArrayList<>();
        Iterator<ChaMAdapterItem<ChaMAdapterValue>> iterator = items.values().iterator();
        while(iterator.hasNext()){
            ChaMAdapterItem<ChaMAdapterValue> item = iterator.next();
            if(item.getSelected()){
                result.add(item.getValue());
            }
        }
        return result;
    }

    public ArrayList<ChaMAdapterValue> all(){
        ArrayList<ChaMAdapterValue> result = new ArrayList<>();
        for(int cursor = 0 ; cursor < getItemCount() ; cursor++){
            result.add(items.get(cursor).getValue());
        }
        return result;
    }

    public void put(ChaMAdapterLoad load, ChaMAdapterValue value){
        if(load == ChaMAdapterLoad.CHAM_ADAPTER_LOAD_UP){
            this.first_key = items.firstKey() == null ? first_key : items.firstKey();
            items.put(first_key,new ChaMAdapterItem<>(first_key,false,value));
        }else{
            this.last_key = items.lastKey() == null ? last_key : items.lastKey();
            items.put(last_key,new ChaMAdapterItem<>(last_key,false,value));
        }
        filter("");
    }

    public enum ChaMAdapterLoad{
        CHAM_ADAPTER_LOAD_UP,
        CHAM_ADAPTER_LOAD_DOWN
    }

    private class ChaMAdapterLayoutManager extends LinearLayoutManager {
        ChaMAdapterLayoutManager(Context context) {
            super(context);
            setSmoothScrollbarEnabled(true);
            setAutoMeasureEnabled(true);
        }

        @Override
        public void onLayoutChildren(RecyclerView.Recycler recycler, RecyclerView.State state) {
            try {
                super.onLayoutChildren(recycler, state);
            } catch (Exception ignored) {}
        }
    }

    private class ChaMAdapterAnimator extends DefaultItemAnimator {
        ChaMAdapterAnimator() {
            setSupportsChangeAnimations(false);
            setAddDuration(0);
            setChangeDuration(0);
            setMoveDuration(0);
            setRemoveDuration(0);
        }
    }

    public static class ChaMAdapterItem<AdapterValue>{
        private String key;
        private boolean selected;
        private AdapterValue value;

        public ChaMAdapterItem(String key, boolean selected, AdapterValue value){
            this.key = key;
            this.selected = selected;
            this.value = value;
        }

        public String getKey(){
            return this.key;
        }
        public boolean getSelected(){
            return this.selected;
        }
        public AdapterValue getValue(){
            return this.value;
        }

        public void setKey(String key){
            this.key = key;
        }
        public void setSelected(boolean selected){
            this.selected = selected;
        }
        public void setValue(AdapterValue value){
            this.value = value;
        }
    }

    public interface ChaMAdapterSearcher<AdapterValue>{
        boolean search(String search, AdapterValue value);
    }

}
