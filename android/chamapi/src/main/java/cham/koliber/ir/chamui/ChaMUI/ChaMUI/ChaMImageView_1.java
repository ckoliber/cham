package cham.koliber.ir.chamui.ChaMUI.ChaMUI;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.drawable.Drawable;
import android.support.annotation.Nullable;
import android.support.v4.graphics.drawable.DrawableCompat;
import android.support.v7.widget.AppCompatImageView;
import android.util.AttributeSet;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.R;

public class ChaMImageView extends AppCompatImageView {

    private int theme_mode;

    public ChaMImageView(Context context) {
        super(context);
        init_view(4);
    }

    public ChaMImageView(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        int theme_mode = 4;
        TypedArray array = getContext().obtainStyledAttributes(attrs, R.styleable.ChaMImageView,0,0);
        if (array.hasValue(R.styleable.ChaMImageView_imageviewThemeMode)) {
            theme_mode = array.getInt(R.styleable.ChaMImageView_imageviewThemeMode, 0);
        }
        array.recycle();
        init_view(theme_mode);
    }

    public ChaMImageView(Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        int theme_mode = 4;
        TypedArray array = getContext().obtainStyledAttributes(attrs, R.styleable.ChaMImageView,0,0);
        if (array.hasValue(R.styleable.ChaMImageView_imageviewThemeMode)) {
            theme_mode = array.getInt(R.styleable.ChaMImageView_imageviewThemeMode, 0);
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
        Drawable drawable;
        switch (theme_mode){
            case 0:
                // MAIN
                drawable = DrawableCompat.wrap(getDrawable());
                DrawableCompat.setTint(drawable,
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_MAIN_ITEM))
                );
                setImageDrawable(drawable);
                break;
            case 1:
                // SEND
                drawable = DrawableCompat.wrap(getDrawable());
                DrawableCompat.setTint(drawable,
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_SEND_ITEM))
                );
                setImageDrawable(drawable);
                break;
            case 2:
                // CHATME
                drawable = DrawableCompat.wrap(getDrawable());
                DrawableCompat.setTint(drawable,
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATME_ITEM))
                );
                setImageDrawable(drawable);
                break;
            case 3:
                // CHATYOU
                drawable = DrawableCompat.wrap(getDrawable());
                DrawableCompat.setTint(drawable,
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATYOU_ITEM))
                );
                setImageDrawable(drawable);
                break;
        }
    }

    @Override
    public void setImageDrawable(@Nullable Drawable drawable) {
        switch (theme_mode){
            case 0:
                // MAIN
                DrawableCompat.setTint(drawable,
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_MAIN_ITEM))
                );
                break;
            case 1:
                // SEND
                DrawableCompat.setTint(drawable,
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_SEND_ITEM))
                );
                break;
            case 2:
                // CHATME
                DrawableCompat.setTint(drawable,
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATME_ITEM))
                );
                break;
            case 3:
                // CHATYOU
                DrawableCompat.setTint(drawable,
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATYOU_ITEM))
                );
                break;
        }
        super.setImageDrawable(drawable);
    }

}
