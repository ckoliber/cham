package cham.koliber.ir.chamui.ChaMUI.ChaMBase;

import android.app.Dialog;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.support.v4.view.ViewCompat;
import android.support.v7.app.AppCompatDialogFragment;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import java.util.Locale;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.R;

public abstract class ChaMDialog extends AppCompatDialogFragment{

    private boolean isRetained = true;
    private ChaMActivity activity;
    private int layout_id = 0;
    private View view;

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        setStyle(STYLE_NO_FRAME, R.style.ChaM_Base_ChaMDialog);
        super.onCreate(savedInstanceState);
        setRetainInstance(true);
        isRetained = false;
    }

    @NonNull
    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {
        return new Dialog(activity,getTheme()){
            @Override
            public boolean dispatchTouchEvent(@NonNull MotionEvent ev) {
                if(getChildFragmentManager().getFragments() != null){
                    Fragment fragment;
                    for(int cursor = getChildFragmentManager().getFragments().size()-1 ; cursor >= 0 ; cursor--){
                        fragment = getChildFragmentManager().getFragments().get(cursor);
                        if(fragment instanceof ChaMFragment){
                            ((ChaMFragment) fragment).onTouch(ev);
                            break;
                        }
                    }
                }
                return super.dispatchTouchEvent(ev);
            }
        };
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        view = inflater.inflate(layout_id, container, false);
        getDialog().getWindow().getAttributes().windowAnimations = R.style.ChaM_Animation_Fade;
        init_dialog();
        init_direction();
        if(!isRetained){
            start_dialog();
            isRetained = true;
        }
        return view;
    }

    @Override
    public void onResume() {
        Runtime.getRuntime().gc();
        System.gc();
        init_size();
        super.onResume();
    }

    private void init_direction(){
        Locale locale = new Locale(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.UI_LANGUAGE));
        final int directionality = Character.getDirectionality(locale.getDisplayName().charAt(0));
        ViewCompat.setLayoutDirection(view , directionality == Character.DIRECTIONALITY_RIGHT_TO_LEFT || directionality == Character.DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC ? ViewCompat.LAYOUT_DIRECTION_RTL : ViewCompat.LAYOUT_DIRECTION_LTR);
    }

    private void init_size(){
        int width = getActivity().getResources().getDisplayMetrics().widthPixels;
        int height = getActivity().getResources().getDisplayMetrics().heightPixels;
        int width_dp = ChaMCoreAPI.Tools.px_to_dp(width);
        if(width_dp > 600){
            getDialog().getWindow().setLayout(ChaMCoreAPI.Tools.dp_to_px(500),height);
        }else{
            getDialog().getWindow().setLayout(width, height);
        }
    }

    public void open_dialog(ChaMActivity activity , int layout_id){
        this.activity = activity;
        this.layout_id = layout_id;
        show(activity.getSupportFragmentManager(),"TAG");
    }

    public void close_dialog(){
        dismiss();
    }

    public ChaMActivity getChaMActivity(){
        return activity;
    }

    public View getChaMView(){
        return view;
    }

    protected abstract void init_dialog();

    protected abstract void start_dialog();

}
