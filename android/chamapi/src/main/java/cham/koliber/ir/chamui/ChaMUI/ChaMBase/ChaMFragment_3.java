package cham.koliber.ir.chamui.ChaMUI.ChaMBase;

import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentTransaction;
import android.support.v4.view.ViewCompat;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import java.util.Locale;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;

public abstract class ChaMFragment extends Fragment{

    private int container_id;
    private boolean isRetained = true;
    private ChaMActivity activity;
    private FragmentManager fragment_manager;
    private boolean swipable;
    private int layout_id = 0;
    private View view;
    private float swipe_x = -1;
    private final float swipe_min = 50;
    private final float swipe_max = 200;

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setRetainInstance(true);
        isRetained = false;
    }

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        view = inflater.inflate(layout_id, container, false);
        init_fragment();
        init_direction();
        if(!isRetained){
            start_fragment();
            isRetained = true;
        }
        return view;
    }

    public void open_fragment(ChaMActivity activity , FragmentManager fragment_manager, boolean swipable , int container_id , int layout_id){
        this.activity = activity;
        this.fragment_manager = fragment_manager;
        this.swipable = swipable;
        this.layout_id = layout_id;
        this.container_id = container_id;
        FragmentTransaction transaction = fragment_manager.beginTransaction();
        transaction.setCustomAnimations(android.R.anim.slide_in_left,android.R.anim.slide_out_right);
        transaction.add(container_id,this).commit();
    }

    public void close_fragment(){
        FragmentTransaction transaction = fragment_manager.beginTransaction();
        transaction.setCustomAnimations(android.R.anim.slide_in_left,android.R.anim.slide_out_right);
        transaction.remove(this).commit();
    }

    private void init_direction(){
        Locale locale = new Locale(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.UI_LANGUAGE));
        final int directionality = Character.getDirectionality(locale.getDisplayName().charAt(0));
        ViewCompat.setLayoutDirection(view , directionality == Character.DIRECTIONALITY_RIGHT_TO_LEFT || directionality == Character.DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC ? ViewCompat.LAYOUT_DIRECTION_RTL : ViewCompat.LAYOUT_DIRECTION_LTR);
    }

    public int getChaMContainerId(){
        return container_id;
    }

    public ChaMActivity getChaMActivity(){
        return activity;
    }

    public FragmentManager getChaMFragmentManager(){
        return fragment_manager;
    }

    public View getChaMView(){
        return view;
    }

    protected abstract void init_fragment();

    protected abstract void start_fragment();

    protected void onTouch(MotionEvent ev){
        if(swipable){
            switch (ev.getAction()){
                case MotionEvent.ACTION_DOWN:
                    if(swipe_x <= 0){
                        swipe_x = ev.getRawX();
                    }
                    break;
                case MotionEvent.ACTION_MOVE:
                    if(swipe_x > 0){
                        float x = ev.getRawX() - swipe_x;
                        if(x > swipe_min){
                            getView().layout((int)(x) , 0 ,(int)(x+getView().getMeasuredWidth()),getView().getMeasuredHeight());
                        }else{
                            getView().layout(0 , 0 ,getView().getMeasuredWidth(),getView().getMeasuredHeight());
                        }
                    }
                    break;
                case MotionEvent.ACTION_UP:
                    if(swipe_x > 0){
                        float x = ev.getRawX() - swipe_x;
                        if(x > swipe_max){
                            close_fragment();
                        }else{
                            getView().layout(0 , 0 , getView().getMeasuredWidth(),getView().getMeasuredHeight());
                        }
                        swipe_x = -1;
                    }
                    break;
            }
        }
    }

}
