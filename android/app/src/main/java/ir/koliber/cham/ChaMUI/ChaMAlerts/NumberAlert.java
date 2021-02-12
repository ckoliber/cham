package ir.koliber.cham.ChaMUI.ChaMAlerts;

import android.support.annotation.NonNull;
import android.support.v7.widget.Toolbar;
import android.view.MenuItem;
import android.view.View;
import android.widget.NumberPicker;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMActivity;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMAlert;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMToolbar;
import ir.koliber.cham.R;

public class NumberAlert extends ChaMAlert{

    private int number_default;
    private int number_min;
    private int number_max;
    private ChaMCoreAPI.ChaMAPIInterface<Integer> api_interface;

    private ChaMToolbar alert_number_toolbar;
    private NumberPicker alert_number_numberpicker;

    public NumberAlert(@NonNull ChaMActivity activity) {
        super(activity);
    }

    public void open_alert(int number_default, int number_min, int number_max , ChaMCoreAPI.ChaMAPIInterface<Integer> api_interface) {
        this.number_default = number_default;
        this.number_min = number_min;
        this.number_max = number_max;
        this.api_interface = api_interface;
        super.open_alert(R.layout.alert_number);
    }

    @Override
    protected void init_alert() {
        init_variables();
        init_toolbar();
        init_content();
    }

    private void init_variables(){
        alert_number_toolbar = getChaMView().findViewById(R.id.alert_number_toolbar);
        alert_number_numberpicker = getChaMView().findViewById(R.id.alert_number_numberpicker);
    }

    private void init_toolbar(){
        alert_number_toolbar.inflateMenu(R.menu.menu_check);
        alert_number_toolbar.setNavigationIcon(getChaMActivity().getResources().getDrawable(R.drawable.ic_back));
        alert_number_toolbar.setNavigationOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                close_alert();
            }
        });
        alert_number_toolbar.setOnMenuItemClickListener(new Toolbar.OnMenuItemClickListener() {
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
        alert_number_numberpicker.setMinValue(number_min);
        alert_number_numberpicker.setMaxValue(number_max);
        alert_number_numberpicker.setValue(number_default);
    }

    private void on_result(){
        if(api_interface != null) {
            api_interface.onResult(alert_number_numberpicker.getValue());
        }
        close_alert();
    }

}
