package cham.koliber.ir.chamui.ChaMUI.ChaMUI;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.TypedArray;
import android.support.annotation.Nullable;
import android.support.v7.widget.CardView;
import android.util.AttributeSet;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.R;

public class ChaMCardView extends CardView {

    private int theme_mode;

    public ChaMCardView(Context context) {
        super(context);
        init_view(0);
    }

    public ChaMCardView(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        int theme_mode = 0;
        TypedArray array = getContext().obtainStyledAttributes(attrs, R.styleable.ChaMCardView,0,0);
        if (array.hasValue(R.styleable.ChaMCardView_cardviewThemeMode)) {
            theme_mode = array.getInt(R.styleable.ChaMCardView_cardviewThemeMode, 0);
        }
        array.recycle();
        init_view(theme_mode);
    }

    public ChaMCardView(Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        int theme_mode = 0;
        TypedArray array = getContext().obtainStyledAttributes(attrs, R.styleable.ChaMCardView,0,0);
        if (array.hasValue(R.styleable.ChaMCardView_cardviewThemeMode)) {
            theme_mode = array.getInt(R.styleable.ChaMCardView_cardviewThemeMode, 0);
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
        // get
        setContentPadding(ChaMCoreAPI.Tools.dp_to_px(3), ChaMCoreAPI.Tools.dp_to_px(3), ChaMCoreAPI.Tools.dp_to_px(3), ChaMCoreAPI.Tools.dp_to_px(3));
        setUseCompatPadding(true);
        setPreventCornerOverlap(true);
        setRadius(3);
        switch (theme_mode){
            case 0:
                // MAIN
                setCardBackgroundColor(
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_MAIN_BACK))
                );
                break;
            case 1:
                // SEND
                setCardBackgroundColor(
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_SEND_BACK))
                );
                break;
            case 2:
                // CHATME
                setCardBackgroundColor(
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATME_BACK))
                );
                break;
            case 3:
                // CHATYOU
                setCardBackgroundColor(
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATYOU_BACK))
                );
                break;
        }
    }

}
