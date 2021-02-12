package ir.koliber.cham.ChaMUI.ChaMAdapters;

import android.support.v7.widget.RecyclerView;
import android.view.View;
import org.json.JSONArray;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMActivity;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMAdapter;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMFab;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMRecyclerView;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMTextView;
import ir.koliber.cham.R;

public class MimesAdapter extends ChaMAdapter<MimesAdapter.MimesAdapterHolder,MimesAdapter.MimesAdapterValue>{

    private MimesAdapterType type;

    public MimesAdapter(ChaMActivity activity, ChaMRecyclerView recycler_view, MimesAdapterType type){
        super(activity,recycler_view,new ChaMAdapterSearcher<MimesAdapterValue>() {
            @Override
            public boolean search(String search, MimesAdapterValue mimesAdapterValue) {
                return mimesAdapterValue.getMimeName().toLowerCase().contains(search.toLowerCase());
            }
        },R.layout.adapter_mimes);
        this.type = type;
    }

    @Override
    protected MimesAdapterHolder create_holder(View view) {
        return new MimesAdapterHolder(view);
    }

    @Override
    protected void init_holder(MimesAdapterHolder holder, final ChaMAdapterItem<MimesAdapterValue> item) {
        holder.adapter_mimes_textview_name.setText(item.getValue().getMimeName());
        holder.adapter_mimes_fab_remove.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v1) {
                remove(item.getKey());
                save();
            }
        });
    }

    @Override
    protected void load_holder(ChaMAdapterLoad load) {
        try{
            JSONArray mimes = new JSONArray(type == MimesAdapterType.MIME_ADAPTER_SET ? ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.STREAM_UPLOAD) : ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.STREAM_DOWNLOAD));
            for(int cursor = 0 ; cursor < mimes.length() ; cursor++){
                String mime_name = mimes.getString(cursor);
                load(load,new MimesAdapterValue(mime_name));
            }
        }catch (Exception ignored){}
        loader(false);
        end(load);
    }

    @Override
    public void put(ChaMAdapterLoad load, MimesAdapterValue mimesAdapterValue) {
        super.put(load, mimesAdapterValue);
        save();
    }

    private void save(){
        JSONArray result = new JSONArray();
        for(MimesAdapterValue value : all()){
            result.put(value.getMimeName());
        }
        ChaMCoreAPI.Interface.self_set(type == MimesAdapterType.MIME_ADAPTER_SET ? ChaMCoreAPI.Interface.ChaMInterfaceKey.STREAM_UPLOAD : ChaMCoreAPI.Interface.ChaMInterfaceKey.STREAM_DOWNLOAD,result.toString());
    }

    class MimesAdapterHolder extends RecyclerView.ViewHolder{

        private View view;
        private ChaMTextView adapter_mimes_textview_name;
        private ChaMFab adapter_mimes_fab_remove;

        MimesAdapterHolder(View view) {
            super(view);
            this.view = view;
            this.adapter_mimes_textview_name = view.findViewById(R.id.adapter_mimes_textview_name);
            this.adapter_mimes_fab_remove = view.findViewById(R.id.adapter_mimes_fab_remove);
        }
    }

    public static class MimesAdapterValue{

        private String mime_name;

        public MimesAdapterValue(String mime_name) {
            this.mime_name = mime_name;
        }

        public String getMimeName() {
            return mime_name;
        }

    }

    public enum MimesAdapterType{
        MIME_ADAPTER_SET,
        MIME_ADAPTER_GET
    }

}
