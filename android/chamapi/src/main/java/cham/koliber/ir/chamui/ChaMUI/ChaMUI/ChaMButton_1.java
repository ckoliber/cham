package cham.koliber.ir.chamui.ChaMUI.ChaMUI;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.ColorStateList;
import android.content.res.TypedArray;
import android.graphics.Typeface;
import android.support.annotation.Nullable;
import android.support.v7.widget.AppCompatButton;
import android.util.AttributeSet;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.R;

public class ChaMButton extends AppCompatButton{

    private int theme_mode;

    public ChaMButton(Context context) {
        super(context);
        init_view(0);
    }

    public ChaMButton(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        int theme_mode = 0;
        TypedArray array = getContext().obtainStyledAttributes(attrs, R.styleable.ChaMButton,0,0);
        if (array.hasValue(R.styleable.ChaMButton_buttonThemeMode)) {
            theme_mode = array.getInt(R.styleable.ChaMButton_buttonThemeMode, 0);
        }
        array.recycle();
        init_view(theme_mode);
    }

    public ChaMButton(Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        int theme_mode = 0;
        TypedArray array = getContext().obtainStyledAttributes(attrs, R.styleable.ChaMButton,0,0);
        if (array.hasValue(R.styleable.ChaMButton_buttonThemeMode)) {
            theme_mode = array.getInt(R.styleable.ChaMButton_buttonThemeMode, 0);
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
        setTypeface(Typeface.DEFAULT_BOLD);
        switch (theme_mode){
            case 0:
                // MAIN
                setSupportBackgroundTintList(ColorStateList.valueOf(
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_MAIN_BACK))
                ));
                setTextColor(
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_MAIN_ITEM))
                );
                break;
            case 1:
                // SEND
                setSupportBackgroundTintList(ColorStateList.valueOf(
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_SEND_BACK))
                ));
                setTextColor(
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_SEND_ITEM))
                );
                break;
        }
    }

}
