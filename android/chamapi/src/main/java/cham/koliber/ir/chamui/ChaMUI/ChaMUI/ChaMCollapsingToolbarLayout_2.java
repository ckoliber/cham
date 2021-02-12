package cham.koliber.ir.chamui.ChaMUI.ChaMUI;

import android.content.Context;
import android.support.design.widget.CollapsingToolbarLayout;
import android.util.AttributeSet;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;

public class ChaMCollapsingToolbarLayout extends CollapsingToolbarLayout{

    public ChaMCollapsingToolbarLayout(Context context) {
        super(context);
        init_view();
    }

    public ChaMCollapsingToolbarLayout(Context context, AttributeSet attrs) {
        super(context, attrs);
        init_view();
    }

    public ChaMCollapsingToolbarLayout(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init_view();
    }

    private void init_view(){
        init_theme();
    }

    private void init_theme(){
        setExpandedTitleColor(
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_TOOLBAR_ITEM))
        );
        setCollapsedTitleTextColor(
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_TOOLBAR_ITEM))
        );
        setBackgroundColor(
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_TOOLBAR_BACK))
        );
        setContentScrimColor(
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_TOOLBAR_BACK))
        );
        setExpandedTitleColor(
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_TOOLBAR_BACK))
        );
    }

}
