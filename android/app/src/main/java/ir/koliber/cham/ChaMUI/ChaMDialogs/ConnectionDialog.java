package ir.koliber.cham.ChaMUI.ChaMDialogs;

import android.support.v4.widget.SwipeRefreshLayout;
import android.view.View;
import android.widget.LinearLayout;
import org.json.JSONObject;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMActivity;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMDialog;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMButton;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMCollapsingToolbarLayout;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMFab;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMSwipeRefreshLayout;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMTextView;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMToolbar;
import ir.koliber.cham.ChaMAPI.ChaMAPI;
import ir.koliber.cham.R;

public class ConnectionDialog extends ChaMDialog{

    private String connection_id;
    private JSONObject connection_infos;
    private ChaMAPI.UserInterface.Permission.ConnectionPermission self_permission;

    private ChaMCollapsingToolbarLayout dialog_connection_collapsingtoolbarlayout;
    private ChaMToolbar dialog_connection_toolbar;
    private ChaMSwipeRefreshLayout dialog_connection_swiperefreshlayout;
    private LinearLayout dialog_connection_linerlayout_link;
    private ChaMTextView dialog_connection_textview_link;
    private LinearLayout dialog_connection_linerlayout_name;
    private ChaMTextView dialog_connection_textview_name;
    private LinearLayout dialog_connection_linerlayout_bio;
    private ChaMTextView dialog_connection_textview_bio;
    private ChaMButton dialog_connection_button_peers;
    private ChaMFab dialog_connection_fab_connection;
    private ChaMFab dialog_connection_fab_picture;

    public void open_dialog(ChaMActivity activity, String connection_id) {
        this.connection_id = connection_id;
        super.open_dialog(activity, R.layout.dialog_connection);
    }

    @Override
    protected void init_dialog() {
        init_variables();
        init_toolbar();
        init_content();
    }

    @Override
    protected void start_dialog() {
        load_content();
    }

    private void init_variables(){
        dialog_connection_collapsingtoolbarlayout = getChaMView().findViewById(R.id.dialog_connection_collapsingtoolbarlayout);
        dialog_connection_toolbar = getChaMView().findViewById(R.id.dialog_connection_toolbar);
        dialog_connection_swiperefreshlayout = getChaMView().findViewById(R.id.dialog_connection_swiperefreshlayout);
        dialog_connection_linerlayout_link = getChaMView().findViewById(R.id.dialog_connection_linerlayout_link);
        dialog_connection_textview_link = getChaMView().findViewById(R.id.dialog_connection_textview_link);
        dialog_connection_linerlayout_name = getChaMView().findViewById(R.id.dialog_connection_linerlayout_name);
        dialog_connection_textview_name = getChaMView().findViewById(R.id.dialog_connection_textview_name);
        dialog_connection_linerlayout_bio = getChaMView().findViewById(R.id.dialog_connection_linerlayout_bio);
        dialog_connection_textview_bio = getChaMView().findViewById(R.id.dialog_connection_textview_bio);
        dialog_connection_button_peers = getChaMView().findViewById(R.id.dialog_connection_button_peers);
        dialog_connection_fab_connection = getChaMView().findViewById(R.id.dialog_connection_fab_connection);
        dialog_connection_fab_picture = getChaMView().findViewById(R.id.dialog_connection_fab_picture);
    }

    private void init_toolbar(){
        dialog_connection_toolbar.setNavigationIcon(getChaMActivity().getResources().getDrawable(R.drawable.ic_back));
        dialog_connection_toolbar.setNavigationOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                close_dialog();
            }
        });
    }

    private void init_content(){
        dialog_connection_linerlayout_link.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if(self_permission != null && self_permission.getLink() == 2){
                    edit_link();
                }
            }
        });
        dialog_connection_linerlayout_name.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if(self_permission != null && self_permission.getName() == 2){
                    edit_name();
                }
            }
        });
        dialog_connection_linerlayout_bio.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if(self_permission != null && self_permission.getBio() == 2){
                    edit_bio();
                }
            }
        });
        dialog_connection_button_peers.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if(self_permission != null && self_permission.getPeers() > 0){
                    ChaMAPI.UserInterface.Items.connection_peers(getChaMActivity(),connection_id,self_permission.toString());
                }
            }
        });
        dialog_connection_fab_connection.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                // open chat
            }
        });
        dialog_connection_fab_picture.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if(self_permission != null && (self_permission.getPictures() == 2 || self_permission.getPictures() == 4)){
                    // open file selector add picture
                }
            }
        });
        dialog_connection_swiperefreshlayout.setOnRefreshListener(new SwipeRefreshLayout.OnRefreshListener() {
            @Override
            public void onRefresh() {
                load_content();
            }
        });
    }

    private void load_content(){
        ChaMCoreAPI.Interface.Protocol.peer_load(null, connection_id, "CONNECTION_INFOS", "", "", "", 0, 1, new ChaMCoreAPI.Interface.Protocol.ProtocolInterface() {
            @Override
            public void onRequest(String token, JSONObject request) {
                dialog_connection_swiperefreshlayout.setRefreshing(true);
            }

            @Override
            public void onError(String error) {
                dialog_connection_swiperefreshlayout.setRefreshing(false);
                ChaMAPI.UserInterface.Output.snack(getChaMView(),getChaMActivity(),dialog_connection_swiperefreshlayout,"Some error occurs !");
            }

            @Override
            public void onResponse(JSONObject response) {
                try{
                    dialog_connection_swiperefreshlayout.setRefreshing(false);
                    ConnectionDialog.this.connection_infos = new JSONObject(response.getString("VALUE"));
                    load_infos();
                }catch (Exception ignored){}
            }
        });
    }

    private void load_infos() throws Exception{
        self_permission = ChaMAPI.UserInterface.Permission.connection(connection_infos.getString("PERMISSION"));
        if(connection_infos.has("LINK")){
            dialog_connection_textview_link.setText(connection_infos.getString("LINK"));
        }
        if(connection_infos.has("NAME")){
            dialog_connection_toolbar.setTitle(connection_infos.getString("NAME"));
            dialog_connection_textview_name.setText(connection_infos.getString("NAME"));
        }
        if(connection_infos.has("BIO")){
            dialog_connection_textview_bio.setText(connection_infos.getString("BIO"));
        }
        if(connection_infos.has("PICTURE")){
            // load picture into imageview
        }
        dialog_connection_linerlayout_link.setVisibility(self_permission.getLink() > 0 ? View.VISIBLE : View.GONE);
        dialog_connection_linerlayout_name.setVisibility(self_permission.getName() > 0 ? View.VISIBLE : View.GONE);
        dialog_connection_linerlayout_bio.setVisibility(self_permission.getBio() > 0 ? View.VISIBLE : View.GONE);
        dialog_connection_button_peers.setVisibility(self_permission.getPeers() > 0 ? View.VISIBLE : View.GONE);
        dialog_connection_fab_connection.setVisibility(self_permission.getMessages() > 0 ? View.VISIBLE : View.GONE);
        dialog_connection_fab_picture.setVisibility((self_permission.getPictures() == 2 || self_permission.getPictures() == 4) ? View.VISIBLE : View.GONE);
    }

    private void edit_link(){
        ChaMAPI.UserInterface.Input.text(getChaMActivity(), dialog_connection_textview_link.getText().toString(), "Enter Link", 5, 20,false , new ChaMCoreAPI.ChaMAPIInterface<String>() {
            @Override
            public void onResult(final String result) {
                ChaMCoreAPI.Interface.Protocol.connection_set(null, connection_id, "LINK", "", result, "SET", new ChaMCoreAPI.Interface.Protocol.ProtocolInterface() {
                    @Override
                    public void onRequest(String token, JSONObject request) {
                        dialog_connection_swiperefreshlayout.setRefreshing(true);
                    }

                    @Override
                    public void onError(String error) {
                        dialog_connection_swiperefreshlayout.setRefreshing(false);
                        ChaMAPI.UserInterface.Output.snack(getChaMView(),getChaMActivity(),dialog_connection_swiperefreshlayout,"Some error occurs !");
                    }

                    @Override
                    public void onResponse(JSONObject response) {
                        try{
                            dialog_connection_swiperefreshlayout.setRefreshing(false);
                            connection_infos.put("LINK",result);
                            load_infos();
                            ChaMAPI.UserInterface.Output.snack(getChaMView(),getChaMActivity(),dialog_connection_swiperefreshlayout,"Link sets !");
                        }catch (Exception ignored){}

                    }
                });
            }
        });
    }

    private void edit_name(){
        ChaMAPI.UserInterface.Input.text(getChaMActivity(), dialog_connection_textview_name.getText().toString(), "Enter Name", 0, 40, false, new ChaMCoreAPI.ChaMAPIInterface<String>() {
            @Override
            public void onResult(final String result) {
                ChaMCoreAPI.Interface.Protocol.connection_set(null, connection_id, "NAME", "", result, "SET", new ChaMCoreAPI.Interface.Protocol.ProtocolInterface() {
                    @Override
                    public void onRequest(String token, JSONObject request) {
                        dialog_connection_swiperefreshlayout.setRefreshing(true);
                    }

                    @Override
                    public void onError(String error) {
                        dialog_connection_swiperefreshlayout.setRefreshing(false);
                        ChaMAPI.UserInterface.Output.snack(getChaMView(),getChaMActivity(),dialog_connection_swiperefreshlayout,"Some error occurs !");
                    }

                    @Override
                    public void onResponse(JSONObject response) {
                        try{
                            dialog_connection_swiperefreshlayout.setRefreshing(false);
                            connection_infos.put("NAME",result);
                            load_infos();
                            ChaMAPI.UserInterface.Output.snack(getChaMView(),getChaMActivity(),dialog_connection_swiperefreshlayout,"Name sets !");
                        }catch (Exception ignored){}

                    }
                });
            }
        });
    }

    private void edit_bio(){
        ChaMAPI.UserInterface.Input.text(getChaMActivity(), dialog_connection_textview_bio.getText().toString(), "Enter Bio", 0, 40, false, new ChaMCoreAPI.ChaMAPIInterface<String>() {
            @Override
            public void onResult(final String result) {
                ChaMCoreAPI.Interface.Protocol.connection_set(null, connection_id, "BIO", "", result, "SET", new ChaMCoreAPI.Interface.Protocol.ProtocolInterface() {
                    @Override
                    public void onRequest(String token, JSONObject request) {
                        dialog_connection_swiperefreshlayout.setRefreshing(true);
                    }

                    @Override
                    public void onError(String error) {
                        dialog_connection_swiperefreshlayout.setRefreshing(false);
                        ChaMAPI.UserInterface.Output.snack(getChaMView(),getChaMActivity(),dialog_connection_swiperefreshlayout,"Some error occurs !");
                    }

                    @Override
                    public void onResponse(JSONObject response) {
                        try{
                            dialog_connection_swiperefreshlayout.setRefreshing(false);
                            connection_infos.put("BIO",result);
                            load_infos();
                            ChaMAPI.UserInterface.Output.snack(getChaMView(),getChaMActivity(),dialog_connection_swiperefreshlayout,"Bio sets !");
                        }catch (Exception ignored){}

                    }
                });
            }
        });
    }

}
