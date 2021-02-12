package cham.koliber.ir.chamui.ChaMUI.ChaMUI;

import android.content.Context;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.util.AttributeSet;
import android.widget.FrameLayout;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;

public class ChaMFrameLayout extends FrameLayout{

    public ChaMFrameLayout(@NonNull Context context) {
        super(context);
        init_view();
    }

    public ChaMFrameLayout(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        init_view();
    }

    public ChaMFrameLayout(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
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
    }

}
