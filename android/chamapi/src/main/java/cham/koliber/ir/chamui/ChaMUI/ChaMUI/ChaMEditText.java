package cham.koliber.ir.chamui.ChaMUI.ChaMUI;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.ColorStateList;
import android.content.res.TypedArray;
import android.support.annotation.Nullable;
import android.util.AttributeSet;
import com.vanniktech.emoji.EmojiEditText;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.R;

public class ChaMEditText extends EmojiEditText{

    private int theme_mode;

    public ChaMEditText(Context context) {
        super(context);
        init_view(0);
    }

    public ChaMEditText(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        int theme_mode = 0;
        TypedArray array = getContext().obtainStyledAttributes(attrs, R.styleable.ChaMEditText,0,0);
        if (array.hasValue(R.styleable.ChaMEditText_edittextThemeMode)) {
            theme_mode = array.getInt(R.styleable.ChaMEditText_edittextThemeMode, 0);
        }
        array.recycle();
        init_view(theme_mode);
    }

    public ChaMEditText(Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs);
        int theme_mode = 0;
        TypedArray array = getContext().obtainStyledAttributes(attrs, R.styleable.ChaMEditText,0,0);
        if (array.hasValue(R.styleable.ChaMEditText_edittextThemeMode)) {
            theme_mode = array.getInt(R.styleable.ChaMEditText_edittextThemeMode, 0);
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
                setTextColor(
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_EDITTEXT_MAIN_ITEM))
                );
                setSupportBackgroundTintList(
                        ColorStateList.valueOf(Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_EDITTEXT_MAIN_BACK)))
                );
                setHintTextColor(
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_EDITTEXT_MAIN_BACK))
                );
                break;
            case 1:
                // SEND
                setTextColor(
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_EDITTEXT_SEND_ITEM))
                );
                setSupportBackgroundTintList(
                        ColorStateList.valueOf(Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_EDITTEXT_SEND_BACK)))
                );
                setHintTextColor(
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_EDITTEXT_SEND_BACK))
                );
                break;
            case 2:
                // TOOLBAR
                setTextColor(
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_EDITTEXT_TOOLBAR_ITEM))
                );
                setSupportBackgroundTintList(
                        ColorStateList.valueOf(Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_EDITTEXT_TOOLBAR_BACK)))
                );
                setHintTextColor(
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_EDITTEXT_TOOLBAR_BACK))
                );
                break;
        }
    }

}
