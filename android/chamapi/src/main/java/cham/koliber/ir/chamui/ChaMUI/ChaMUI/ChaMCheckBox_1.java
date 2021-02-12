package cham.koliber.ir.chamui.ChaMUI.ChaMUI;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.ColorStateList;
import android.content.res.TypedArray;
import android.os.Build;
import android.support.annotation.Nullable;
import android.support.v4.widget.CompoundButtonCompat;
import android.support.v7.widget.AppCompatCheckBox;
import android.util.AttributeSet;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.R;

public class ChaMCheckBox extends AppCompatCheckBox{

    private int theme_mode;

    public ChaMCheckBox(Context context) {
        super(context);
        init_view(0);
    }

    public ChaMCheckBox(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        int theme_mode = 0;
        TypedArray array = getContext().obtainStyledAttributes(attrs, R.styleable.ChaMCheckBox,0,0);
        if (array.hasValue(R.styleable.ChaMCheckBox_checkboxThemeMode)) {
            theme_mode = array.getInt(R.styleable.ChaMCheckBox_checkboxThemeMode, 0);
        }
        array.recycle();
        init_view(theme_mode);
    }

    public ChaMCheckBox(Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        int theme_mode = 0;
        TypedArray array = getContext().obtainStyledAttributes(attrs, R.styleable.ChaMCheckBox,0,0);
        if (array.hasValue(R.styleable.ChaMCheckBox_checkboxThemeMode)) {
            theme_mode = array.getInt(R.styleable.ChaMCheckBox_checkboxThemeMode, 0);
        }
        array.recycle();
        init_view(theme_mode);
    }

    public void init_view(int theme_mode) {
        this.theme_mode = theme_mode;
        init_theme();
    }

    @SuppressLint("RestrictedApi")
    private void init_theme(){
        switch (theme_mode){
            case 0:
                // MAIN
                if(Build.VERSION.SDK_INT < 21){
                    CompoundButtonCompat.setButtonTintList(this, ColorStateList.valueOf(
                            Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_MAIN_ITEM))
                    ));
                }else{
                    setButtonTintList(ColorStateList.valueOf(
                            Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_MAIN_ITEM))
                    ));
                }
                break;
            case 1:
                // SEND
                if(Build.VERSION.SDK_INT < 21){
                    CompoundButtonCompat.setButtonTintList(this, ColorStateList.valueOf(
                            Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_SEND_ITEM))
                    ));
                }else{
                    setButtonTintList(ColorStateList.valueOf(
                            Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_SEND_ITEM))
                    ));
                }
                break;
            case 2:
                // CHATME
                if(Build.VERSION.SDK_INT < 21){
                    CompoundButtonCompat.setButtonTintList(this, ColorStateList.valueOf(
                            Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATME_ITEM))
                    ));
                }else{
                    setButtonTintList(ColorStateList.valueOf(
                            Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATME_ITEM))
                    ));
                }
                break;
            case 3:
                // CHATYOU
                if(Build.VERSION.SDK_INT < 21){
                    CompoundButtonCompat.setButtonTintList(this, ColorStateList.valueOf(
                            Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATYOU_ITEM))
                    ));
                }else{
                    setButtonTintList(ColorStateList.valueOf(
                            Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATYOU_ITEM))
                    ));
                }
                break;
        }
    }

}
