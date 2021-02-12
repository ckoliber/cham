package cham.koliber.ir.chamui.ChaMUI.ChaMBase;

import android.support.annotation.NonNull;
import android.support.v4.view.ViewCompat;
import android.support.v7.app.AlertDialog;
import android.view.LayoutInflater;
import android.view.View;
import java.util.Locale;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.R;

public abstract class ChaMAlert extends AlertDialog{

    private ChaMActivity activity;
    private View view;

    protected ChaMAlert(@NonNull ChaMActivity activity) {
        super(activity);
        this.activity = activity;
    }

    public void open_alert(int layout_id){
        LayoutInflater inflater = getLayoutInflater();
        getWindow().getAttributes().windowAnimations = R.style.ChaM_Animation_Fade;
        view = inflater.inflate(layout_id, null);
        init_alert();
        init_direction();
        setView(view);
        show();
    }

    public void close_alert(){
        dismiss();
    }

    private void init_direction(){
        Locale locale = new Locale(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.UI_LANGUAGE));
        final int directionality = Character.getDirectionality(locale.getDisplayName().charAt(0));
        ViewCompat.setLayoutDirection(view , directionality == Character.DIRECTIONALITY_RIGHT_TO_LEFT || directionality == Character.DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC ? ViewCompat.LAYOUT_DIRECTION_RTL : ViewCompat.LAYOUT_DIRECTION_LTR);
    }

    public ChaMActivity getChaMActivity(){
        return activity;
    }

    public View getChaMView(){
        return view;
    }

    protected abstract void init_alert();
}
