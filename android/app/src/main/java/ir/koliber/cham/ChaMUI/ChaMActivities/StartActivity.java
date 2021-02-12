package ir.koliber.cham.ChaMUI.ChaMActivities;

import android.content.Intent;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMActivity;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMToolbar;
import ir.koliber.cham.ChaMUI.ChaMDialogs.ItemsDialog;
import ir.koliber.cham.R;

public class StartActivity extends ChaMActivity {

    private ChaMToolbar activity_start_toolbar;

    @Override
    public void init_activity() {
        if(ChaMCoreAPI.Interface.state() > 0 && (ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.UI_PIN).length() <= 0 || ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.UI_LOCK).equals("0"))){
            startActivity(new Intent(getApplicationContext(),ChatActivity.class));
            overridePendingTransition(android.R.anim.fade_in,android.R.anim.fade_out);
            finish();
        }else{
            setContentView(R.layout.activity_start);
            init_variables();
            init_toolbar();
            init_content();
        }
    }

    @Override
    public void start_activity() {
        if(ChaMCoreAPI.Interface.state() > 0){
//            PinFragment pin_fragment = new PinFragment();
//            pin_fragment.open_fragment(this,R.id.activity_start_framelayout);
        }else{
//            startActivity(new Intent(getApplicationContext(),IntroActivity.class));
//            overridePendingTransition(android.R.anim.slide_in_left,android.R.anim.slide_out_right);
//            ConnectionDialog d = new ConnectionDialog();
//            d.open_dialog(this,"TEST");
//
//            PeerDialog p = new PeerDialog();
//            p.open_dialog(this,"TEST");

//            PeerCreateDialog pcd = new PeerCreateDialog();
//            pcd.open_dialog(this);
//
//            ConnectionCreateDialog ccd = new ConnectionCreateDialog();
//            ccd.open_dialog(this);
//
//            SettingsDialog sd = new SettingsDialog();
//            sd.open_dialog(this);

//            NumberAlert na = new NumberAlert(this);
//            na.open_alert(0,10,100,null);

//            TextAlert ta = new TextAlert(this);
//            ta.open_alert("","",0,100,true,null);

//            PermissionAlert pa = new PermissionAlert(this);
//            pa.open_alert("",null);

//            ChaMAPI.Progress.open(ChaMAPI.Progress.ProgressType.PROGRESS_TYPE_BANDWIDTH,true);


//            PermissionAlert pa = new PermissionAlert(this);
//            pa.open_alert("222222222",null);

            ItemsDialog sd = new ItemsDialog();

        }
    }

    private void init_variables(){
        activity_start_toolbar = findViewById(R.id.activity_start_toolbar);
    }

    private void init_toolbar(){
        activity_start_toolbar.bringToFront();
    }

    private void init_content(){

    }

}
