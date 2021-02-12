package cham.koliber.ir.chamui.ChaMUI.ChaMUI;

import android.content.Context;
import android.content.res.ColorStateList;
import android.content.res.TypedArray;
import android.graphics.PorterDuff;
import android.os.Build;
import android.util.AttributeSet;
import android.widget.ProgressBar;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.R;

public class ChaMProgress extends ProgressBar{

    private int theme_mode;

    public ChaMProgress(Context context) {
        super(context);
        init_view(0);
    }

    public ChaMProgress(Context context, AttributeSet attrs) {
        super(context, attrs);
        int theme_mode = 0;
        TypedArray array = getContext().obtainStyledAttributes(attrs, R.styleable.ChaMProgress,0,0);
        if (array.hasValue(R.styleable.ChaMProgress_progressThemeMode)) {
            theme_mode = array.getInt(R.styleable.ChaMProgress_progressThemeMode, 0);
        }
        array.recycle();
        init_view(theme_mode);
    }

    public ChaMProgress(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        int theme_mode = 0;
        TypedArray array = getContext().obtainStyledAttributes(attrs, R.styleable.ChaMProgress,0,0);
        if (array.hasValue(R.styleable.ChaMProgress_progressThemeMode)) {
            theme_mode = array.getInt(R.styleable.ChaMProgress_progressThemeMode, 0);
        }
        array.recycle();
        init_view(theme_mode);
    }

    private void init_view(int theme_mode){
        this.theme_mode = theme_mode;
        init_theme();
    }

    private void init_theme(){
        switch (theme_mode){
            case 0:
                // MAIN
                if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP){
                    setProgressTintList(ColorStateList.valueOf(
                            Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_MAIN_BACK))
                    ));
                    setSecondaryProgressTintList(ColorStateList.valueOf(
                            Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_MAIN_BACK))
                    ));
                    setIndeterminateTintList(ColorStateList.valueOf(
                            Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_MAIN_BACK))
                    ));
                }else{
                    PorterDuff.Mode mode = PorterDuff.Mode.SRC_IN;
                    if(Build.VERSION.SDK_INT <= Build.VERSION_CODES.GINGERBREAD_MR1){
                        mode = PorterDuff.Mode.MULTIPLY;
                    }
                    if(getIndeterminateDrawable() != null){
                        getIndeterminateDrawable().setColorFilter(
                                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_MAIN_BACK))
                                , mode);
                    }
                    if(getProgressDrawable() != null){
                        getProgressDrawable().setColorFilter(
                                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_MAIN_BACK))
                                , mode);
                    }
                }
                break;
            case 1:
                // SEND
                if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP){
                    setProgressTintList(ColorStateList.valueOf(
                            Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_SEND_BACK))
                    ));
                    setSecondaryProgressTintList(ColorStateList.valueOf(
                            Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_SEND_BACK))
                    ));
                    setIndeterminateTintList(ColorStateList.valueOf(
                            Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_SEND_BACK))
                    ));
                }else{
                    PorterDuff.Mode mode = PorterDuff.Mode.SRC_IN;
                    if(Build.VERSION.SDK_INT <= Build.VERSION_CODES.GINGERBREAD_MR1){
                        mode = PorterDuff.Mode.MULTIPLY;
                    }
                    if(getIndeterminateDrawable() != null){
                        getIndeterminateDrawable().setColorFilter(
                                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_SEND_BACK))
                                , mode);
                    }
                    if(getProgressDrawable() != null){
                        getProgressDrawable().setColorFilter(
                                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_SEND_BACK))
                                , mode);
                    }
                }
                break;
        }
    }
}
