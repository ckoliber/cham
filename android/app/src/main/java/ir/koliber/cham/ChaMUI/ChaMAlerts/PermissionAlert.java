package ir.koliber.cham.ChaMUI.ChaMAlerts;

import android.support.annotation.NonNull;
import android.support.v7.widget.Toolbar;
import android.view.MenuItem;
import android.view.View;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMActivity;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMAlert;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMCardView;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMSpinner;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMTextView;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMToolbar;
import ir.koliber.cham.ChaMAPI.ChaMAPI;
import ir.koliber.cham.R;

public class PermissionAlert extends ChaMAlert{

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//            % Every Peer have :
//            %   id name         type  pn  permissions
//            %   1. ID           TEXT  -
//            %   2. SCODE        TEXT  -
//            %   3. PHONE        TEXT  A   [0:hidden,1:visible,2:visible&change]
//            %   4. LINK         TEXT  B   [0:hidden,1:visible,2:visible&change]
//            %   5. NAME         TEXT  C   [0:hidden,1:visible,2:visible&change]
//            %   6. BIO          TEXT  D   [0:hidden,1:visible,2:visible&change]
//            %   7. STATE        TEXT  E   [0:hidden,1:visible,2:visible&change]
//            %   8. PICTURES     MAP{[date : link]}                    F   [0:hidden,1:visible,2:visible&add,3:visible&remove,4:visible&add&remove]
//            %   9. PEERS  MAP{[peer_id : permission]}                 G   [0:hidden,1:visible,2:visible&add,3:visible&remove,4:visible&add&remove]
//            %     (get function : contains : range type : _ : any)
//            %  10. CONNECTIONS  MAP{[connection_id : date]}           H   [0:hidden,1:visible,2:visible&add,3:visible&remove,4:visible&add&remove]
//            %  11. SOCKETS      MAP{[socket:node]}                    -
//            %  PERMISSION : ABCDEFGH
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//            % Every Connection have :
//            %   id name         type  pn  permissions
//            %   1. ID           TEXT  -
//            %   2. LINK         TEXT  A   [0:hidden,1:visible,2:visible&change]
//            %   3. NAME         TEXT  B   [0:hidden,1:visible,2:visible&change]
//            %   4. BIO          TEXT  C   [0:hidden,1:visible,2:visible&change]
//            %   5. PICTURES     MAP{[date : link]}                    D   [0:hidden,1:visible,2:visible&add,3:visible&remove,4:visible&add&remove]
//            %   6. PEERS        MAP{[peer_id : permission+F]}         E   [0:hidden,1:visible,2:visible&add,3:visible&remove,4:visible&add&remove] // add : boss !!!,have a default peer : * -> default permission for join if have'nt -> can't join !!!
//            %   7. HISTORIES    MAP{[peer_id : date]}                 -   have no permission to see or change,when user removed from PEERS add date to this field for message loading
//            %      MESSAGES                                           F   [0:noread_nowrite,1:read_nowrite,2:noread_write,3:read_write,4:read_write_delete]
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    private PermissionAlertType type;
    private String permission;
    private ChaMCoreAPI.ChaMAPIInterface<String> api_interface;

    private ChaMToolbar alert_permission_toolbar;
    private ChaMTextView alert_permission_textview_permission1;
    private ChaMSpinner alert_permission_spinner_permission1;
    private ChaMTextView alert_permission_textview_permission2;
    private ChaMSpinner alert_permission_spinner_permission2;
    private ChaMTextView alert_permission_textview_permission3;
    private ChaMSpinner alert_permission_spinner_permission3;
    private ChaMTextView alert_permission_textview_permission4;
    private ChaMSpinner alert_permission_spinner_permission4;
    private ChaMTextView alert_permission_textview_permission5;
    private ChaMSpinner alert_permission_spinner_permission5;
    private ChaMTextView alert_permission_textview_permission6;
    private ChaMSpinner alert_permission_spinner_permission6;
    private ChaMTextView alert_permission_textview_permission7;
    private ChaMSpinner alert_permission_spinner_permission7;
    private ChaMTextView alert_permission_textview_permission8;
    private ChaMSpinner alert_permission_spinner_permission8;
    private ChaMCardView alert_permission_cardview_permission7;
    private ChaMCardView alert_permission_cardview_permission8;

    public PermissionAlert(@NonNull ChaMActivity activity) {
        super(activity);
    }

    public void open_alert(PermissionAlertType type, String permission, ChaMCoreAPI.ChaMAPIInterface<String> api_interface){
        this.type = type;
        this.permission = permission;
        this.api_interface = api_interface;
        super.open_alert(R.layout.alert_permission);
    }

    @Override
    protected void init_alert() {
        init_variables();
        init_toolbar();
        init_content();
    }

    private void init_variables(){
        alert_permission_toolbar = getChaMView().findViewById(R.id.alert_permission_toolbar);
        alert_permission_textview_permission1 = getChaMView().findViewById(R.id.alert_permission_textview_permission1);
        alert_permission_spinner_permission1 = getChaMView().findViewById(R.id.alert_permission_spinner_permission1);
        alert_permission_textview_permission2 = getChaMView().findViewById(R.id.alert_permission_textview_permission2);
        alert_permission_spinner_permission2 = getChaMView().findViewById(R.id.alert_permission_spinner_permission2);
        alert_permission_textview_permission3 = getChaMView().findViewById(R.id.alert_permission_textview_permission3);
        alert_permission_spinner_permission3 = getChaMView().findViewById(R.id.alert_permission_spinner_permission3);
        alert_permission_textview_permission4 = getChaMView().findViewById(R.id.alert_permission_textview_permission4);
        alert_permission_spinner_permission4 = getChaMView().findViewById(R.id.alert_permission_spinner_permission4);
        alert_permission_textview_permission5 = getChaMView().findViewById(R.id.alert_permission_textview_permission5);
        alert_permission_spinner_permission5 = getChaMView().findViewById(R.id.alert_permission_spinner_permission5);
        alert_permission_textview_permission6 = getChaMView().findViewById(R.id.alert_permission_textview_permission6);
        alert_permission_spinner_permission6 = getChaMView().findViewById(R.id.alert_permission_spinner_permission6);
        alert_permission_textview_permission7 = getChaMView().findViewById(R.id.alert_permission_textview_permission7);
        alert_permission_spinner_permission7 = getChaMView().findViewById(R.id.alert_permission_spinner_permission7);
        alert_permission_textview_permission8 = getChaMView().findViewById(R.id.alert_permission_textview_permission8);
        alert_permission_spinner_permission8 = getChaMView().findViewById(R.id.alert_permission_spinner_permission8);
        alert_permission_cardview_permission7 = getChaMView().findViewById(R.id.alert_permission_cardview_permission7);
        alert_permission_cardview_permission8 = getChaMView().findViewById(R.id.alert_permission_cardview_permission8);
    }

    private void init_toolbar(){
        alert_permission_toolbar.inflateMenu(R.menu.menu_check);
        alert_permission_toolbar.setNavigationIcon(getChaMActivity().getResources().getDrawable(R.drawable.ic_back));
        alert_permission_toolbar.setNavigationOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                close_alert();
            }
        });
        alert_permission_toolbar.setOnMenuItemClickListener(new Toolbar.OnMenuItemClickListener() {
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
        String permissions1[] = new String[]{
                "Hidden",
                "Visible",
                "Visible & Change"
        };
        String permissions2[] = new String[]{
                "Hidden",
                "Visible",
                "Visible & Add",
                "Visible & Remove",
                "Visible & Add & Remove"
        };
        String permissions3[] = new String[]{
                "NoRead & NoWrite",
                "Read & NoWrite",
                "Write & NoRead",
                "Write & Read",
                "Write & Read & Delete"
        };
        if(permission.length() == 9){
            ChaMAPI.UserInterface.Permission.PeerPermission peer_permission = new ChaMAPI.UserInterface.Permission.PeerPermission(permission);
            alert_permission_spinner_permission1.setData(permissions1);
            alert_permission_spinner_permission2.setData(permissions1);
            alert_permission_spinner_permission3.setData(permissions1);
            alert_permission_spinner_permission4.setData(permissions1);
            alert_permission_spinner_permission5.setData(permissions1);
            alert_permission_spinner_permission6.setData(permissions2);
            alert_permission_spinner_permission7.setData(permissions2);
            alert_permission_spinner_permission8.setData(permissions2);
            alert_permission_textview_permission1.setText("Phone : ");
            alert_permission_textview_permission2.setText("Link : ");
            alert_permission_textview_permission3.setText("Name : ");
            alert_permission_textview_permission4.setText("Bio : ");
            alert_permission_textview_permission5.setText("State : ");
            alert_permission_textview_permission6.setText("Pictures : ");
            alert_permission_textview_permission7.setText("Peer : ");
            alert_permission_textview_permission8.setText("Connections : ");
            alert_permission_spinner_permission1.setSelection(peer_permission.getPhone());
            alert_permission_spinner_permission2.setSelection(peer_permission.getLink());
            alert_permission_spinner_permission3.setSelection(peer_permission.getName());
            alert_permission_spinner_permission4.setSelection(peer_permission.getBio());
            alert_permission_spinner_permission5.setSelection(peer_permission.getState());
            alert_permission_spinner_permission6.setSelection(peer_permission.getPictures());
            alert_permission_spinner_permission7.setSelection(peer_permission.getPeers());
            alert_permission_spinner_permission8.setSelection(peer_permission.getConnections());
        }else if(permission.length() == 6){
            ChaMAPI.UserInterface.Permission.ConnectionPermission connection_permission = new ChaMAPI.UserInterface.Permission.ConnectionPermission(permission);
            alert_permission_cardview_permission7.setVisibility(View.GONE);
            alert_permission_cardview_permission8.setVisibility(View.GONE);
            alert_permission_spinner_permission1.setData(permissions1);
            alert_permission_spinner_permission2.setData(permissions1);
            alert_permission_spinner_permission3.setData(permissions1);
            alert_permission_spinner_permission4.setData(permissions2);
            alert_permission_spinner_permission5.setData(permissions2);
            alert_permission_spinner_permission6.setData(permissions3);
            alert_permission_textview_permission1.setText("Link : ");
            alert_permission_textview_permission2.setText("Name : ");
            alert_permission_textview_permission3.setText("Bio : ");
            alert_permission_textview_permission4.setText("Pictures : ");
            alert_permission_textview_permission5.setText("Peers : ");
            alert_permission_textview_permission6.setText("Messages : ");
            alert_permission_spinner_permission1.setSelection(connection_permission.getLink());
            alert_permission_spinner_permission2.setSelection(connection_permission.getName());
            alert_permission_spinner_permission3.setSelection(connection_permission.getBio());
            alert_permission_spinner_permission4.setSelection(connection_permission.getPictures());
            alert_permission_spinner_permission5.setSelection(connection_permission.getPeers());
            alert_permission_spinner_permission6.setSelection(connection_permission.getMessages());
        }
        alert_permission_spinner_permission1.setEnabled(type == PermissionAlertType.PERMISSION_ALERT_EDIT);
        alert_permission_spinner_permission2.setEnabled(type == PermissionAlertType.PERMISSION_ALERT_EDIT);
        alert_permission_spinner_permission3.setEnabled(type == PermissionAlertType.PERMISSION_ALERT_EDIT);
        alert_permission_spinner_permission4.setEnabled(type == PermissionAlertType.PERMISSION_ALERT_EDIT);
        alert_permission_spinner_permission5.setEnabled(type == PermissionAlertType.PERMISSION_ALERT_EDIT);
        alert_permission_spinner_permission6.setEnabled(type == PermissionAlertType.PERMISSION_ALERT_EDIT);
        alert_permission_spinner_permission7.setEnabled(type == PermissionAlertType.PERMISSION_ALERT_EDIT);
        alert_permission_spinner_permission8.setEnabled(type == PermissionAlertType.PERMISSION_ALERT_EDIT);
    }

    private void on_result(){
        if(api_interface != null) {
            if(permission.length() == 9){
                api_interface.onResult(
                        alert_permission_spinner_permission1.getSelectedItemPosition()+""+
                                alert_permission_spinner_permission2.getSelectedItemPosition()+""+
                                alert_permission_spinner_permission3.getSelectedItemPosition()+""+
                                alert_permission_spinner_permission4.getSelectedItemPosition()+""+
                                alert_permission_spinner_permission5.getSelectedItemPosition()+""+
                                alert_permission_spinner_permission6.getSelectedItemPosition()+""+
                                alert_permission_spinner_permission7.getSelectedItemPosition()+""+
                                alert_permission_spinner_permission8.getSelectedItemPosition()
                );
            }else if(permission.length() == 6){
                api_interface.onResult(
                        alert_permission_spinner_permission1.getSelectedItemPosition()+""+
                                alert_permission_spinner_permission2.getSelectedItemPosition()+""+
                                alert_permission_spinner_permission3.getSelectedItemPosition()+""+
                                alert_permission_spinner_permission4.getSelectedItemPosition()+""+
                                alert_permission_spinner_permission5.getSelectedItemPosition()+""+
                                alert_permission_spinner_permission6.getSelectedItemPosition()
                );
            }
        }
        close_alert();
    }

    public enum PermissionAlertType{
        PERMISSION_ALERT_SHOW,
        PERMISSION_ALERT_EDIT
    }

}
