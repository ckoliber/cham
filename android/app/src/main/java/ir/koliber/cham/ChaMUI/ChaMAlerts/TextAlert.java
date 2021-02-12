package ir.koliber.cham.ChaMUI.ChaMAlerts;

import android.support.annotation.NonNull;
import android.support.design.widget.TextInputLayout;
import android.support.v7.widget.Toolbar;
import android.text.method.DigitsKeyListener;
import android.view.MenuItem;
import android.view.View;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMActivity;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMAlert;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMEditText;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMToolbar;
import ir.koliber.cham.ChaMAPI.ChaMAPI;
import ir.koliber.cham.R;

public class TextAlert extends ChaMAlert{

    private String text_default;
    private String text_hint;
    private int text_min;
    private int text_max;
    private boolean text_numeric;
    private ChaMCoreAPI.ChaMAPIInterface<String> api_interface;

    private ChaMToolbar alert_text_toolbar;
    private TextInputLayout alert_text_textinputlayout;
    private ChaMEditText alert_text_edittext;

    public TextAlert(@NonNull ChaMActivity activity) {
        super(activity);
    }

    public void open_alert(String text_default , String text_hint , int text_min , int text_max , boolean text_numeric , ChaMCoreAPI.ChaMAPIInterface<String> api_interface) {
        this.text_default = text_default;
        this.text_hint = text_hint;
        this.text_min = text_min;
        this.text_max = text_max;
        this.text_numeric = text_numeric;
        this.api_interface = api_interface;
        super.open_alert(R.layout.alert_text);
    }

    @Override
    protected void init_alert() {
        init_variables();
        init_toolbar();
        init_content();
    }

    private void init_variables(){
        alert_text_toolbar = getChaMView().findViewById(R.id.alert_text_toolbar);
        alert_text_textinputlayout = getChaMView().findViewById(R.id.alert_text_textinputlayout);
        alert_text_edittext = getChaMView().findViewById(R.id.alert_text_edittext);
    }

    private void init_toolbar(){
        alert_text_toolbar.inflateMenu(R.menu.menu_check);
        alert_text_toolbar.setNavigationIcon(getChaMActivity().getResources().getDrawable(R.drawable.ic_back));
        alert_text_toolbar.setNavigationOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                close_alert();
            }
        });
        alert_text_toolbar.setOnMenuItemClickListener(new Toolbar.OnMenuItemClickListener() {
            @Override
            public boolean onMenuItemClick(MenuItem item) {
                if(item.getItemId() == R.id.menu_check_check){
                    on_result();
                }
                return false;
            }
        });
    }

    private void init_content(){
        alert_text_edittext.setText(text_default);
        alert_text_edittext.setHint(text_hint);
        if(text_max > 0){
            alert_text_textinputlayout.setCounterEnabled(true);
            alert_text_textinputlayout.setCounterMaxLength(text_max);
        }
        if(text_numeric){
            alert_text_edittext.setKeyListener(DigitsKeyListener.getInstance("0123456789"));
        }
    }

    private void on_result(){
        if(alert_text_edittext.getText().toString().length() < text_min){
            ChaMAPI.UserInterface.Output.snack(getChaMView(),getChaMActivity(),getChaMView(),"MIN ERROR");
            return;
        }
        if(alert_text_edittext.getText().toString().length() > text_max){
            ChaMAPI.UserInterface.Output.snack(getChaMView(),getChaMActivity(),getChaMView(),"MAX ERROR");
            return;
        }
        if(api_interface != null){
            api_interface.onResult(alert_text_edittext.getText().toString());
        }
        close_alert();
    }

}
