package cham.koliber.ir.chamui.ChaMUI.ChaMUI;

import android.content.Context;
import android.content.res.ColorStateList;
import android.content.res.TypedArray;
import android.graphics.drawable.Drawable;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.design.widget.FloatingActionButton;
import android.support.v4.graphics.drawable.DrawableCompat;
import android.util.AttributeSet;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.R;

public class ChaMFab extends FrameLayout{

    private int mode;
    private int theme_mode;
    private int src;

    private View view;
    private FloatingActionButton cham_fab_fab;
    private ProgressBar cham_fab_progressbar;

    public ChaMFab(@NonNull Context context) {
        super(context);
        init_view(0,0,0);
    }

    public ChaMFab(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        int mode = 0;
        int theme_mode = 0;
        int src = 0;
        TypedArray array = getContext().obtainStyledAttributes(attrs, R.styleable.ChaMFab,0,0);
        if (array.hasValue(R.styleable.ChaMFab_fabMode)) {
            mode = array.getInt(R.styleable.ChaMFab_fabMode, 0);
        }
        if (array.hasValue(R.styleable.ChaMFab_fabThemeMode)) {
            theme_mode = array.getInt(R.styleable.ChaMFab_fabThemeMode, 0);
        }
        if (array.hasValue(R.styleable.ChaMFab_fabSrc)) {
            src = array.getResourceId(R.styleable.ChaMFab_fabSrc, 0);
        }
        array.recycle();
        init_view(mode,theme_mode,src);
    }

    public ChaMFab(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        int mode = 0;
        int theme_mode = 0;
        int src = 0;
        TypedArray array = getContext().obtainStyledAttributes(attrs, R.styleable.ChaMFab,0,0);
        if (array.hasValue(R.styleable.ChaMFab_fabMode)) {
            mode = array.getInt(R.styleable.ChaMFab_fabMode, 0);
        }
        if (array.hasValue(R.styleable.ChaMFab_fabThemeMode)) {
            theme_mode = array.getInt(R.styleable.ChaMFab_fabThemeMode, 0);
        }
        if (array.hasValue(R.styleable.ChaMFab_fabSrc)) {
            src = array.getResourceId(R.styleable.ChaMFab_fabSrc, 0);
        }
        array.recycle();
        init_view(mode,theme_mode,src);
    }

    private void init_view(int mode, int theme_mode, int src){
        this.mode = mode;
        this.theme_mode = theme_mode;
        this.src = src;
        this.view = inflate(getContext(), R.layout.cham_fab,this);
        init_variables();
        init_content();
        init_theme();
    }

    private void init_variables(){
        cham_fab_fab = view.findViewById(R.id.cham_fab_fab);
        cham_fab_progressbar = view.findViewById(R.id.cham_fab_progressbar);
    }

    private void init_content(){
        if(src > 0){
            setIcon(getResources().getDrawable(src));
        }
        if(mode == 0){
            cham_fab_fab.setSize(FloatingActionButton.SIZE_MINI);
            cham_fab_progressbar.setLayoutParams(new LinearLayout.LayoutParams(ChaMCoreAPI.Tools.dp_to_px(60),ChaMCoreAPI.Tools.dp_to_px(60)));
        }else{
            cham_fab_fab.setSize(FloatingActionButton.SIZE_NORMAL);
            cham_fab_progressbar.setLayoutParams(new LinearLayout.LayoutParams(ChaMCoreAPI.Tools.dp_to_px(80),ChaMCoreAPI.Tools.dp_to_px(80)));
        }
    }

    private void init_theme(){
        Drawable drawable = DrawableCompat.wrap(cham_fab_fab.getDrawable());
        switch (theme_mode){
            case 0:
                // MAIN
                cham_fab_fab.setBackgroundTintList(ColorStateList.valueOf(
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_MAIN_BACK))
                ));
                DrawableCompat.setTint(drawable,
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_MAIN_ITEM))
                );
                break;
            case 1:
                // SEND
                cham_fab_fab.setBackgroundTintList(ColorStateList.valueOf(
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_SEND_BACK))
                ));
                DrawableCompat.setTint(drawable,
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_SEND_ITEM))
                );
                break;
        }
        cham_fab_fab.setImageDrawable(drawable);
    }

    public void setIcon(Drawable drawable){
        switch (theme_mode){
            case 0:
                // MAIN
                DrawableCompat.setTint(drawable,
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_MAIN_ITEM))
                );
                break;
            case 1:
                // SEND
                DrawableCompat.setTint(drawable,
                        Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_SEND_ITEM))
                );
                break;
        }
        cham_fab_fab.setImageDrawable(drawable);
    }

    public void showProgress(){
        cham_fab_progressbar.setVisibility(VISIBLE);
    }

    public void hideProgress(){
        cham_fab_progressbar.setVisibility(GONE);
    }

    public void setProgress(int progress){
        if(progress < 0){
            cham_fab_progressbar.setIndeterminate(true);
        }else{
            cham_fab_progressbar.setIndeterminate(false);
            cham_fab_progressbar.setProgress(progress);
        }
    }

    public void setIndeterminate(boolean indeterminate){
        cham_fab_progressbar.setIndeterminate(indeterminate);
    }

    public void setOnClickListener(View.OnClickListener listener){
        cham_fab_fab.setOnClickListener(listener);
    }
}
