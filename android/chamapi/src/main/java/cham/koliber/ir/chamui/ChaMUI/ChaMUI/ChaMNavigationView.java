package cham.koliber.ir.chamui.ChaMUI.ChaMUI;

import android.content.Context;
import android.content.res.ColorStateList;
import android.support.design.widget.NavigationView;
import android.util.AttributeSet;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;

public class ChaMNavigationView extends NavigationView{

    public ChaMNavigationView(Context context) {
        super(context);
        init_view();
    }

    public ChaMNavigationView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init_view();
    }

    public ChaMNavigationView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init_view();
    }

    private void init_view(){
        init_theme();
    }

    private void init_theme(){
        setBackgroundColor(
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_MAIN_BACK))
        );
        setItemIconTintList(new ColorStateList(new int[][] {
                new int[] { android.R.attr.state_enabled}, // enabled
                new int[] {-android.R.attr.state_enabled}, // disabled
                new int[] {-android.R.attr.state_checked}, // unchecked
                new int[] { android.R.attr.state_pressed}  // pressed
        }, new int[] {
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_MAIN_ITEM)),
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_MAIN_ITEM)),
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_MAIN_ITEM)),
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_MAIN_ITEM))
        }));
        setItemTextColor(new ColorStateList(new int[][] {
                new int[] { android.R.attr.state_enabled}, // enabled
                new int[] {-android.R.attr.state_enabled}, // disabled
                new int[] {-android.R.attr.state_checked}, // unchecked
                new int[] { android.R.attr.state_pressed}  // pressed
        }, new int[] {
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_MAIN_ITEM)),
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_MAIN_ITEM)),
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_MAIN_ITEM)),
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_MAIN_ITEM))
        }));
    }
}
