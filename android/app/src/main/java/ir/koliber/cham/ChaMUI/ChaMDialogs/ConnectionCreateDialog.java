package ir.koliber.cham.ChaMUI.ChaMDialogs;

import android.support.v4.app.FragmentManager;
import android.view.View;
import android.widget.AdapterView;
import org.json.JSONObject;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMActivity;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMDialog;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMFragment;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMButton;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMSpinner;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMSwipeRefreshLayout;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMToolbar;
import ir.koliber.cham.ChaMAPI.ChaMAPI;
import ir.koliber.cham.R;

public class ConnectionCreateDialog extends ChaMDialog{

    private static ChaMCoreAPI.ChaMAPIInterface<String> api_interface;

    private ChaMToolbar dialog_connectioncreate_toolbar;

    public void open_dialog(ChaMActivity activity, ChaMCoreAPI.ChaMAPIInterface<String> api_interface) {
        ConnectionCreateDialog.api_interface = api_interface;
        super.open_dialog(activity, R.layout.dialog_connectioncreate);
    }

    @Override
    protected void init_dialog() {
        init_variables();
        init_toolbar();
        init_content();
    }

    @Override
    protected void start_dialog() {
        ConnectionCreateTypeFragment connectioncreatetype_fragment = new ConnectionCreateTypeFragment();
        connectioncreatetype_fragment.open_fragment(getChaMActivity(),getChildFragmentManager(),R.id.dialog_connectioncreate_framelayout);
    }

    private void init_variables(){
        dialog_connectioncreate_toolbar = getChaMView().findViewById(R.id.dialog_connectioncreate_toolbar);
    }

    private void init_toolbar(){
        dialog_connectioncreate_toolbar.setNavigationIcon(getChaMActivity().getResources().getDrawable(R.drawable.ic_back));
        dialog_connectioncreate_toolbar.setNavigationOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                close_dialog();
            }
        });
    }

    private void init_content(){}

    public static class ConnectionCreateTypeFragment extends ChaMFragment {

        /**
         *  Every Connection have two basic permission :
         *      _ : default peers (not member) permission
         *      * : default join permission
         **/

        private String[] types = new String[]{"Peer Chat","Group Chat","Channel Chat","Custom Chat"};
        private String peer_permission;
        private String join_permission;

        private ChaMSwipeRefreshLayout fragment_connectioncreatetype_swiperefreshlayout;
        private ChaMSpinner fragment_connectioncreatetype_spinner_type;
        private ChaMButton fragment_connectioncreatetype_button_peerpermission;
        private ChaMButton fragment_connectioncreatetype_button_joinpermission;
        private ChaMButton fragment_connectioncreatetype_button_create;

        public void open_fragment(ChaMActivity activity, FragmentManager fragment_manager, int container_id) {
            super.open_fragment(activity,fragment_manager,false,container_id,R.layout.fragment_connectioncreatetype);
        }

        @Override
        protected void init_fragment() {
            init_variables();
            init_content();
        }

        @Override
        protected void start_fragment() {
            fragment_connectioncreatetype_spinner_type.setData(types);
        }

        private void init_variables(){
            fragment_connectioncreatetype_swiperefreshlayout = getChaMView().findViewById(R.id.fragment_connectioncreatetype_swiperefreshlayout);
            fragment_connectioncreatetype_spinner_type = getChaMView().findViewById(R.id.fragment_connectioncreatetype_spinner_type);
            fragment_connectioncreatetype_button_peerpermission = getChaMView().findViewById(R.id.fragment_connectioncreatetype_button_peerpermission);
            fragment_connectioncreatetype_button_joinpermission = getChaMView().findViewById(R.id.fragment_connectioncreatetype_button_joinpermission);
            fragment_connectioncreatetype_button_create = getChaMView().findViewById(R.id.fragment_connectioncreatetype_button_create);
        }

        private void init_content(){
            fragment_connectioncreatetype_spinner_type.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
                @Override
                public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                    switch (position){
                        case 0:
                            fragment_connectioncreatetype_button_peerpermission.setEnabled(false);
                            fragment_connectioncreatetype_button_joinpermission.setEnabled(false);
                            peer_permission = "000000";
                            join_permission = "222444";
                            break;
                        case 1:
                            fragment_connectioncreatetype_button_peerpermission.setEnabled(false);
                            fragment_connectioncreatetype_button_joinpermission.setEnabled(false);
                            peer_permission = "111100";
                            join_permission = "111113";
                            break;
                        case 2:
                            fragment_connectioncreatetype_button_peerpermission.setEnabled(false);
                            fragment_connectioncreatetype_button_joinpermission.setEnabled(false);
                            peer_permission = "111100";
                            join_permission = "111101";
                            break;
                        case 3:
                            fragment_connectioncreatetype_button_peerpermission.setEnabled(true);
                            fragment_connectioncreatetype_button_joinpermission.setEnabled(true);
                            peer_permission = "000000";
                            join_permission = "000000";
                            break;
                    }
                }

                @Override
                public void onNothingSelected(AdapterView<?> parent) {}
            });
            fragment_connectioncreatetype_button_peerpermission.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    ChaMAPI.UserInterface.Output.permission(peer_permission, true, new ChaMCoreAPI.ChaMAPIInterface<String>() {
                        @Override
                        public void onResult(String result) {
                            peer_permission = result;
                        }
                    });
                }
            });
            fragment_connectioncreatetype_button_joinpermission.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    ChaMAPI.UserInterface.Output.permission(join_permission, true, new ChaMCoreAPI.ChaMAPIInterface<String>() {
                        @Override
                        public void onResult(String result) {
                            join_permission = result;
                        }
                    });
                }
            });
            fragment_connectioncreatetype_button_create.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    create_connection();
                }
            });
            fragment_connectioncreatetype_swiperefreshlayout.setEnabled(false);
        }

        private void create_connection(){
            ChaMCoreAPI.Interface.Protocol.connection_create(null, new ChaMCoreAPI.Interface.Protocol.ProtocolInterface() {
                @Override
                public void onRequest(String token, JSONObject request) {
                    fragment_connectioncreatetype_button_create.setEnabled(false);
                    fragment_connectioncreatetype_swiperefreshlayout.setRefreshing(true);

                }

                @Override
                public void onError(String error) {
                    fragment_connectioncreatetype_button_create.setEnabled(true);
                    fragment_connectioncreatetype_swiperefreshlayout.setRefreshing(false);
                    ChaMAPI.UserInterface.Output.snack(fragment_connectioncreatetype_swiperefreshlayout,getChaMActivity(),getChaMActivity().getCurrentFocus(),"Some error occurs !");
                }

                @Override
                public void onResponse(JSONObject response) {
                    try{
                        init_connection(response.getString("ID"));
                    }catch (Exception ignored){}
                }
            });
        }

        private void init_connection(final String id){
            ChaMCoreAPI.Interface.Protocol.connection_set(null, id, "PEERS", "_", peer_permission, "ADD", new ChaMCoreAPI.Interface.Protocol.ProtocolInterface() {
                @Override
                public void onRequest(String token, JSONObject request) {}

                @Override
                public void onError(String error) {
                    fragment_connectioncreatetype_button_create.setEnabled(true);
                    fragment_connectioncreatetype_swiperefreshlayout.setRefreshing(false);
                    ChaMAPI.UserInterface.Output.snack(fragment_connectioncreatetype_swiperefreshlayout,getChaMActivity(),getChaMActivity().getCurrentFocus(),"Some error occurs !");
                }

                @Override
                public void onResponse(JSONObject response) {
                    ChaMCoreAPI.Interface.Protocol.connection_set(null, id, "PEERS", "*", join_permission, "ADD", new ChaMCoreAPI.Interface.Protocol.ProtocolInterface() {
                        @Override
                        public void onRequest(String token, JSONObject request) {}

                        @Override
                        public void onError(String error) {
                            fragment_connectioncreatetype_button_create.setEnabled(true);
                            fragment_connectioncreatetype_swiperefreshlayout.setRefreshing(false);
                            ChaMAPI.UserInterface.Output.snack(fragment_connectioncreatetype_swiperefreshlayout,getChaMActivity(),getChaMActivity().getCurrentFocus(),"Some error occurs !");
                        }

                        @Override
                        public void onResponse(JSONObject response) {
                            fragment_connectioncreatetype_button_create.setEnabled(true);
                            fragment_connectioncreatetype_swiperefreshlayout.setRefreshing(false);
                            if(api_interface != null){
                                api_interface.onResult(id);
                            }
                        }
                    });
                }
            });
        }
    }

}
