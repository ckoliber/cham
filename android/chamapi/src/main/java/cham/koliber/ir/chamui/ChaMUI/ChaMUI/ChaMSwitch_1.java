package cham.koliber.ir.chamui.ChaMUI.ChaMUI;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.ColorStateList;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.support.annotation.Nullable;
import android.support.v4.graphics.drawable.DrawableCompat;
import android.support.v7.widget.SwitchCompat;
import android.util.AttributeSet;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.R;

public class ChaMSwitch extends SwitchCompat {

    private int theme_mode;

    public ChaMSwitch(Context context) {
        super(context);
        init_view(0);
    }

    public ChaMSwitch(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        int theme_mode = 0;
        TypedArray array = getContext().obtainStyledAttributes(attrs, R.styleable.ChaMSwitch,0,0);
        if (array.hasValue(R.styleable.ChaMSwitch_switchThemeMode)) {
            theme_mode = array.getInt(R.styleable.ChaMSwitch_switchThemeMode, 0);
        }
        array.recycle();
        init_view(theme_mode);
    }

    public ChaMSwitch(Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        int theme_mode = 0;
        TypedArray array = getContext().obtainStyledAttributes(attrs, R.styleable.ChaMSwitch,0,0);
        if (array.hasValue(R.styleable.ChaMSwitch_switchThemeMode)) {
            theme_mode = array.getInt(R.styleable.ChaMSwitch_switchThemeMode, 0);
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
                DrawableCompat.setTintList(getThumbDrawable(), new ColorStateList(
                        new int[][]{
                                new int[]{android.R.attr.state_checked},
                                new int[]{}
                        },
                        new int[]{
                                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_MAIN_ITEM)),
                                Color.WHITE
                        }));
                DrawableCompat.setTintList(getTrackDrawable(), new ColorStateList(
                        new int[][]{
                                new int[]{android.R.attr.state_checked},
                                new int[]{}
                        },
                        new int[]{
                                Color.parseColor("#FFCCCCCC"),
                                Color.parseColor("#4D000000")
                        })
                );
                break;
            case 1:
                // SEND
                DrawableCompat.setTintList(getThumbDrawable(), new ColorStateList(
                        new int[][]{
                                new int[]{android.R.attr.state_checked},
                                new int[]{}
                        },
                        new int[]{
                                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_SEND_ITEM)),
                                Color.WHITE
                        }));
                DrawableCompat.setTintList(getTrackDrawable(), new ColorStateList(
                        new int[][]{
                                new int[]{android.R.attr.state_checked},
                                new int[]{}
                        },
                        new int[]{
                                Color.parseColor("#FFCCCCCC"),
                                Color.parseColor("#4D000000")
                        })
                );
                break;
            case 2:
                // CHATME
                DrawableCompat.setTintList(getThumbDrawable(), new ColorStateList(
                        new int[][]{
                                new int[]{android.R.attr.state_checked},
                                new int[]{}
                        },
                        new int[]{
                                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATME_ITEM)),
                                Color.WHITE
                        }));
                DrawableCompat.setTintList(getTrackDrawable(), new ColorStateList(
                        new int[][]{
                                new int[]{android.R.attr.state_checked},
                                new int[]{}
                        },
                        new int[]{
                                Color.parseColor("#FFCCCCCC"),
                                Color.parseColor("#4D000000")
                        })
                );
                break;
            case 3:
                // CHATYOU
                DrawableCompat.setTintList(getThumbDrawable(), new ColorStateList(
                        new int[][]{
                                new int[]{android.R.attr.state_checked},
                                new int[]{}
                        },
                        new int[]{
                                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATYOU_ITEM)),
                                Color.WHITE
                        }));
                DrawableCompat.setTintList(getTrackDrawable(), new ColorStateList(
                        new int[][]{
                                new int[]{android.R.attr.state_checked},
                                new int[]{}
                        },
                        new int[]{
                                Color.parseColor("#FFCCCCCC"),
                                Color.parseColor("#4D000000")
                        })
                );
                break;
        }
    }

}
