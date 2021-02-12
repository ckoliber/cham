package cham.koliber.ir.chamui.ChaMUI.ChaMUI;

import android.content.Context;
import android.support.design.widget.AppBarLayout;
import android.util.AttributeSet;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;

public class ChaMAppBarLayout extends AppBarLayout{

    public ChaMAppBarLayout(Context context) {
        super(context);
        init_view();
    }

    public ChaMAppBarLayout(Context context, AttributeSet attrs) {
        super(context, attrs);
        init_view();
    }

    private void init_view(){
        init_theme();
    }

    private void init_theme(){
        setBackgroundColor(
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_TOOLBAR_BACK))
        );
    }

}
