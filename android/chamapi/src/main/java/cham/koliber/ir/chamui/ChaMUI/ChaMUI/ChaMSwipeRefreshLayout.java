package cham.koliber.ir.chamui.ChaMUI.ChaMUI;

import android.content.Context;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.widget.SwipeRefreshLayout;
import android.util.AttributeSet;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.R;

public class ChaMSwipeRefreshLayout extends SwipeRefreshLayout{

    public ChaMSwipeRefreshLayout(@NonNull Context context) {
        super(context);
        init_view();
    }

    public ChaMSwipeRefreshLayout(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        init_view();
    }

    public ChaMSwipeRefreshLayout(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs);
        init_view();
    }

    private void init_view(){
        init_theme();
    }

    private void init_theme(){
        setBackgroundColor(
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_MAIN_BACK))
        );
        setColorSchemeResources(R.color.Red7,R.color.Green7,R.color.Blue7);
    }

}
