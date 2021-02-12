package cham.koliber.ir.chamui.ChaMUI.ChaMUI;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.drawable.Drawable;
import android.support.annotation.Nullable;
import android.support.v4.graphics.drawable.DrawableCompat;
import android.support.v7.widget.Toolbar;
import android.util.AttributeSet;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;

public class ChaMToolbar extends Toolbar{

    public ChaMToolbar(Context context) {
        super(context);
        init_view();
    }

    public ChaMToolbar(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        init_view();
    }

    public ChaMToolbar(Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs);
        init_view();
    }

    private void init_view(){
        init_theme();
    }

    @SuppressLint("RestrictedApi")
    private void init_theme(){
        // get
        setBackgroundColor(
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_TOOLBAR_BACK))
        );
        setTitleTextColor(
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_TOOLBAR_ITEM))
        );
        setSubtitleTextColor(
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_TOOLBAR_ITEM))
        );
        for(int cursor = 0 ; cursor < getMenu().size() ; cursor++){
            Drawable drawable = getMenu().getItem(cursor).getIcon();
            DrawableCompat.setTint(drawable,
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_TOOLBAR_ITEM))
            );
            getMenu().getItem(cursor).setIcon(drawable);
        }
        Drawable drawable = getNavigationIcon();
        if(drawable == null){return;}
        DrawableCompat.setTint(drawable,
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_TOOLBAR_ITEM))
        );
        setNavigationIcon(drawable);
    }

    @Override
    public void setNavigationIcon(@Nullable Drawable icon) {
        DrawableCompat.setTint(icon,
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_TOOLBAR_ITEM))
        );
        super.setNavigationIcon(icon);
    }

    @Override
    public void inflateMenu(int resId) {
        super.inflateMenu(resId);
        for(int cursor = 0 ; cursor < getMenu().size() ; cursor++){
            Drawable drawable = getMenu().getItem(cursor).getIcon();
            DrawableCompat.setTint(drawable,
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_TOOLBAR_ITEM))
            );
            getMenu().getItem(cursor).setIcon(drawable);
        }
    }
}
