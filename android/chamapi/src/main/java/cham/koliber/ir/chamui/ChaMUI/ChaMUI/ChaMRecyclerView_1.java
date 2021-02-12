package cham.koliber.ir.chamui.ChaMUI.ChaMUI;

import android.content.Context;
import android.support.annotation.Nullable;
import android.support.v4.widget.SwipeRefreshLayout;
import android.support.v7.widget.RecyclerView;
import android.util.AttributeSet;
import android.view.View;
import android.widget.FrameLayout;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.R;

public class ChaMRecyclerView extends FrameLayout{

    private View view;
    private SwipeRefreshLayout cham_recyclerview_swiperefreshlayout;
    private RecyclerView cham_recyclerview_recyclerview;

    public ChaMRecyclerView(Context context) {
        super(context);
        init_view();
    }

    public ChaMRecyclerView(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        init_view();
    }

    public ChaMRecyclerView(Context context, @Nullable AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
        init_view();
    }

    private void init_view(){
        this.view = inflate(getContext(), R.layout.cham_recyclerview,this);
        init_variables();
        init_content();
        init_theme();
    }

    private void init_variables(){
        cham_recyclerview_swiperefreshlayout = view.findViewById(R.id.cham_recyclerview_swiperefreshlayout);
        cham_recyclerview_recyclerview = view.findViewById(R.id.cham_recyclerview_recyclerview);
    }

    private void init_content(){
        cham_recyclerview_swiperefreshlayout.setEnabled(false);
    }

    private void init_theme(){
        setBackgroundColor(
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_MAIN_BACK))
        );
        cham_recyclerview_swiperefreshlayout.setBackgroundColor(
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_MAIN_BACK))
        );
        cham_recyclerview_recyclerview.setBackgroundColor(
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_MAIN_BACK))
        );
    }

    public void setRefreshing(boolean refreshing){
        cham_recyclerview_swiperefreshlayout.setRefreshing(refreshing);
    }

    public void setAdapter(RecyclerView.Adapter adapter){
        cham_recyclerview_recyclerview.setAdapter(adapter);
    }

    public RecyclerView.LayoutManager getLayoutManager(){
        return cham_recyclerview_recyclerview.getLayoutManager();
    }

    public void setLayoutManager(RecyclerView.LayoutManager layout){
        cham_recyclerview_recyclerview.setLayoutManager(layout);
    }

    public void removeOnScrollListener(RecyclerView.OnScrollListener listener){
        cham_recyclerview_recyclerview.removeOnScrollListener(listener);
    }

    public void addOnScrollListener(RecyclerView.OnScrollListener listener){
        cham_recyclerview_recyclerview.addOnScrollListener(listener);
    }

    public void setItemAnimator(RecyclerView.ItemAnimator animator){
        cham_recyclerview_recyclerview.setItemAnimator(animator);
    }

    public void setNestedScrollingEnabled(boolean enabled){
        cham_recyclerview_recyclerview.setNestedScrollingEnabled(enabled);
    }

    public void setHasFixedSize(boolean hasFixedSize){
        cham_recyclerview_recyclerview.setHasFixedSize(hasFixedSize);
    }

    public void setItemViewCacheSize(int size){
        cham_recyclerview_recyclerview.setItemViewCacheSize(size);
    }

    public void setDrawingCacheEnabled(boolean enabled){
        cham_recyclerview_recyclerview.setDrawingCacheEnabled(enabled);
    }

    public void setLayoutFrozen(boolean frozen){
        cham_recyclerview_recyclerview.setLayoutFrozen(frozen);
    }

    public void setWillNotCacheDrawing(boolean willNotCacheDrawing){
        cham_recyclerview_recyclerview.setWillNotCacheDrawing(willNotCacheDrawing);
    }

    public void setDrawingCacheQuality(int quality){
        cham_recyclerview_recyclerview.setDrawingCacheQuality(quality);
    }

}
