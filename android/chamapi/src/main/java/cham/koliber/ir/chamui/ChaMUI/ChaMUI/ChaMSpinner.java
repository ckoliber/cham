package cham.koliber.ir.chamui.ChaMUI.ChaMUI;

import android.content.Context;
import android.content.res.Resources;
import android.content.res.TypedArray;
import android.graphics.drawable.ColorDrawable;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v7.widget.AppCompatSpinner;
import android.util.AttributeSet;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.R;

public class ChaMSpinner extends AppCompatSpinner{

    private int theme_mode;

    public ChaMSpinner(Context context) {
        super(context);
        init_view(0);
    }

    public ChaMSpinner(Context context, int mode) {
        super(context, mode);
        init_view(0);
    }

    public ChaMSpinner(Context context, AttributeSet attrs) {
        super(context, attrs);
        int theme_mode = 0;
        TypedArray array = getContext().obtainStyledAttributes(attrs, R.styleable.ChaMSpinner,0,0);
        if (array.hasValue(R.styleable.ChaMSpinner_spinnerThemeMode)) {
            theme_mode = array.getInt(R.styleable.ChaMSpinner_spinnerThemeMode, 0);
        }
        array.recycle();
        init_view(theme_mode);
    }

    public ChaMSpinner(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        int theme_mode = 0;
        TypedArray array = getContext().obtainStyledAttributes(attrs, R.styleable.ChaMSpinner,0,0);
        if (array.hasValue(R.styleable.ChaMSpinner_spinnerThemeMode)) {
            theme_mode = array.getInt(R.styleable.ChaMSpinner_spinnerThemeMode, 0);
        }
        array.recycle();
        init_view(theme_mode);
    }

    public ChaMSpinner(Context context, AttributeSet attrs, int defStyleAttr, int mode) {
        super(context, attrs, defStyleAttr, mode);
        int theme_mode = 0;
        TypedArray array = getContext().obtainStyledAttributes(attrs, R.styleable.ChaMSpinner,0,0);
        if (array.hasValue(R.styleable.ChaMSpinner_spinnerThemeMode)) {
            theme_mode = array.getInt(R.styleable.ChaMSpinner_spinnerThemeMode, 0);
        }
        array.recycle();
        init_view(theme_mode);
    }

    public ChaMSpinner(Context context, AttributeSet attrs, int defStyleAttr, int mode, Resources.Theme popupTheme) {
        super(context, attrs, defStyleAttr, mode, popupTheme);
        int theme_mode = 0;
        TypedArray array = getContext().obtainStyledAttributes(attrs, R.styleable.ChaMSpinner,0,0);
        if (array.hasValue(R.styleable.ChaMSpinner_spinnerThemeMode)) {
            theme_mode = array.getInt(R.styleable.ChaMSpinner_spinnerThemeMode, 0);
        }
        array.recycle();
        init_view(theme_mode);
    }

    private void init_view(int theme_mode){
        this.theme_mode = theme_mode;
        init_theme();
    }

    private void init_theme(){
        setPopupBackgroundDrawable(new ColorDrawable(
                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_MAIN_BACK))
        ));
    }

    public void setData(String data[]){
        ArrayAdapter<String> adapter = new ArrayAdapter<String>(getContext(), R.layout.cham_spinneritem, data){
            @NonNull
            @Override
            public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
                ChaMTextView cham_spinneritem_textview = (ChaMTextView) super.getView(position, convertView, parent);
                switch (theme_mode){
                    case 0:
                        // MAIN
                        cham_spinneritem_textview.setTextColor(
                                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_MAIN_ITEM))
                        );
                        break;
                    case 1:
                        // SEND
                        cham_spinneritem_textview.setTextColor(
                                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_SEND_ITEM))
                        );
                        break;
                    case 2:
                        // CHATME
                        cham_spinneritem_textview.setTextColor(
                                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATME_ITEM))
                        );
                        break;
                    case 3:
                        // CHATYOU
                        cham_spinneritem_textview.setTextColor(
                                Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATYOU_ITEM))
                        );
                        break;
                }
                return cham_spinneritem_textview;
            }
        };
        adapter.setDropDownViewResource(R.layout.cham_spinnerdropitem);
        setAdapter(adapter);
    }

}
