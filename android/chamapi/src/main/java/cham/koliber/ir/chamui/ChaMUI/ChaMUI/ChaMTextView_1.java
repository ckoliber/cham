package cham.koliber.ir.chamui.ChaMUI.ChaMUI;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.TypedArray;
import android.support.annotation.Nullable;
import android.util.AttributeSet;
import com.vanniktech.emoji.EmojiTextView;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.R;

public class ChaMTextView extends EmojiTextView {

    private int theme_mode;

    public ChaMTextView(Context context) {
        super(context);
        init_view(0);
    }

    public ChaMTextView(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        int theme_mode = 0;
        TypedArray array = getContext().obtainStyledAttributes(attrs, R.styleable.ChaMTextView,0,0);
        if (array.hasValue(R.styleable.ChaMTextView_textviewThemeMode)) {
            theme_mode = array.getInt(R.styleable.ChaMTextView_textviewThemeMode, 0);
        }
        array.recycle();
        init_view(theme_mode);
    }

    public ChaMTextView(Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs);
        int theme_mode = 0;
        TypedArray array = getContext().obtainStyledAttributes(attrs, R.styleable.ChaMTextView,0,0);
        if (array.hasValue(R.styleable.ChaMTextView_textviewThemeMode)) {
            theme_mode = array.getInt(R.styleable.ChaMTextView_textviewThemeMode, 0);
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
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_MAIN_ITEM))
                );
                break;
            case 1:
                // SEND
                setTextColor(
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_SEND_ITEM))
                );
                break;
            case 2:
                // CHATME
                setTextColor(
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATME_ITEM))
                );
                setTextSize(
                        Float.parseFloat(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_WINDOW_CHAT_FONT))
                );
                break;
            case 3:
                // CHATYOU
                setTextColor(
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATYOU_ITEM))
                );
                setTextSize(
                        Float.parseFloat(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_WINDOW_CHAT_FONT))
                );
                break;
        }
    }

}
