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

public class PeerDialog extends ChaMDialog{

    private String peer_id;
    private JSONObject peer_infos;
    private ChaMAPI.UserInterface.Permission.PeerPermission self_permission;

    private ChaMCollapsingToolbarLayout dialog_peer_collapsingtoolbarlayout;
    private ChaMToolbar dialog_peer_toolbar;
    private ChaMSwipeRefreshLayout dialog_peer_swiperefreshlayout;

    private LinearLayout dialog_peer_linerlayout_phone;
    private ChaMTextView dialog_peer_textview_phone;
    private LinearLayout dialog_peer_linerlayout_link;
    private ChaMTextView dialog_peer_textview_link;
    private LinearLayout dialog_peer_linerlayout_name;
    private ChaMTextView dialog_peer_textview_name;
    private LinearLayout dialog_peer_linerlayout_bio;
    private ChaMTextView dialog_peer_textview_bio;
    private ChaMButton dialog_peer_button_peers;
    private ChaMButton dialog_peer_button_connections;
    private ChaMFab dialog_peer_fab_connection;
    private ChaMFab dialog_peer_fab_picture;

    public void open_dialog(ChaMActivity activity, String peer_id) {
        this.peer_id = peer_id;
        super.open_dialog(activity, R.layout.dialog_peer);
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
        dialog_peer_collapsingtoolbarlayout = getChaMView().findViewById(R.id.dialog_peer_collapsingtoolbarlayout);
        dialog_peer_toolbar = getChaMView().findViewById(R.id.dialog_peer_toolbar);
        dialog_peer_swiperefreshlayout = getChaMView().findViewById(R.id.dialog_peer_swiperefreshlayout);
        dialog_peer_linerlayout_phone = getChaMView().findViewById(R.id.dialog_peer_linerlayout_phone);
        dialog_peer_textview_phone = getChaMView().findViewById(R.id.dialog_peer_textview_phone);
        dialog_peer_linerlayout_link = getChaMView().findViewById(R.id.dialog_peer_linerlayout_link);
        dialog_peer_textview_link = getChaMView().findViewById(R.id.dialog_peer_textview_link);
        dialog_peer_linerlayout_name = getChaMView().findViewById(R.id.dialog_peer_linerlayout_name);
        dialog_peer_textview_name = getChaMView().findViewById(R.id.dialog_peer_textview_name);
        dialog_peer_linerlayout_bio = getChaMView().findViewById(R.id.dialog_peer_linerlayout_bio);
        dialog_peer_textview_bio = getChaMView().findViewById(R.id.dialog_peer_textview_bio);
        dialog_peer_button_peers = getChaMView().findViewById(R.id.dialog_peer_button_peers);
        dialog_peer_button_connections = getChaMView().findViewById(R.id.dialog_peer_button_connections);
        dialog_peer_fab_connection = getChaMView().findViewById(R.id.dialog_peer_fab_connection);
        dialog_peer_fab_picture = getChaMView().findViewById(R.id.dialog_peer_fab_picture);
    }

    private void init_toolbar(){
        dialog_peer_toolbar.setNavigationIcon(getChaMActivity().getResources().getDrawable(R.drawable.ic_back));
        dialog_peer_toolbar.setNavigationOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                close_dialog();
            }
        });
    }

    private void init_content(){
        dialog_peer_linerlayout_phone.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if(self_permission != null && self_permission.getPhone() == 2){
                    edit_phone();
                }
            }
        });
        dialog_peer_linerlayout_link.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if(self_permission != null && self_permission.getLink() == 2){
                    edit_link();
                }
            }
        });
        dialog_peer_linerlayout_name.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if(self_permission != null && self_permission.getName() == 2){
                    edit_name();
                }
            }
        });
        dialog_peer_linerlayout_bio.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if(self_permission != null && self_permission.getBio() == 2){
                    edit_bio();
                }
            }
        });
        dialog_peer_button_peers.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if(self_permission != null && self_permission.getPeers() > 0){
                    ChaMAPI.UserInterface.Items.peer_peers(getChaMActivity(),peer_id,self_permission.toString());
                }
            }
        });
        dialog_peer_button_connections.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if(self_permission != null && self_permission.getConnections() > 0){
                    ChaMAPI.UserInterface.Items.peer_connections(getChaMActivity(),peer_id,self_permission.toString());
                }
            }
        });
        dialog_peer_fab_connection.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                // open chat
            }
        });
        dialog_peer_fab_picture.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if(self_permission != null && (self_permission.getPictures() == 2 || self_permission.getPictures() == 4)){
                    // open file selector add picture
                }
            }
        });
        dialog_peer_swiperefreshlayout.setOnRefreshListener(new SwipeRefreshLayout.OnRefreshListener() {
            @Override
            public void onRefresh() {
                load_content();
            }
        });
    }

    private void load_content(){
        ChaMCoreAPI.Interface.Protocol.peer_load(null, peer_id, "PEER_INFOS", "", "", "", 0, 1, new ChaMCoreAPI.Interface.Protocol.ProtocolInterface() {
            @Override
            public void onRequest(String token, JSONObject request) {
                dialog_peer_swiperefreshlayout.setRefreshing(true);
            }

            @Override
            public void onError(String error) {
                dialog_peer_swiperefreshlayout.setRefreshing(false);
                ChaMAPI.UserInterface.Output.snack(getChaMView(),getChaMActivity(),dialog_peer_swiperefreshlayout,"Some error occurs !");
            }

            @Override
            public void onResponse(JSONObject response) {
                try{
                    dialog_peer_swiperefreshlayout.setRefreshing(false);
                    PeerDialog.this.peer_infos = new JSONObject(response.getString("VALUE"));
                    load_infos();
                }catch (Exception ignored){}
            }
        });
    }

    private void load_infos() throws Exception{
        self_permission = ChaMAPI.UserInterface.Permission.peer(peer_infos.getString("PERMISSION"));
        if(peer_infos.has("PHONE")){
            dialog_peer_textview_phone.setText(peer_infos.getString("PHONE"));
        }
        if(peer_infos.has("LINK")){
            dialog_peer_textview_link.setText(peer_infos.getString("LINK"));
        }
        if(peer_infos.has("NAME")){
            dialog_peer_toolbar.setTitle(peer_infos.getString("NAME"));
            dialog_peer_textview_name.setText(peer_infos.getString("NAME"));
        }
        if(peer_infos.has("BIO")){
            dialog_peer_textview_bio.setText(peer_infos.getString("BIO"));
        }
        if(peer_infos.has("STATE")){
            dialog_peer_toolbar.setSubtitle(peer_infos.getString("STATE"));
        }
        if(peer_infos.has("PICTURE")){
            // load picture into imageview
        }
        dialog_peer_linerlayout_phone.setVisibility(self_permission.getPhone() > 0 ? View.VISIBLE : View.GONE);
        dialog_peer_linerlayout_link.setVisibility(self_permission.getLink() > 0 ? View.VISIBLE : View.GONE);
        dialog_peer_linerlayout_name.setVisibility(self_permission.getName() > 0 ? View.VISIBLE : View.GONE);
        dialog_peer_linerlayout_bio.setVisibility(self_permission.getBio() > 0 ? View.VISIBLE : View.GONE);
        dialog_peer_button_peers.setVisibility(self_permission.getPeers() > 0 ? View.VISIBLE : View.GONE);
        dialog_peer_button_connections.setVisibility(self_permission.getConnections() > 0 ? View.VISIBLE : View.GONE);
        dialog_peer_fab_connection.setVisibility(View.VISIBLE);
        dialog_peer_fab_picture.setVisibility((self_permission.getPictures() == 2 || self_permission.getPictures() == 4) ? View.VISIBLE : View.GONE);
    }

    private void edit_phone(){}

    private void edit_link(){
        ChaMAPI.UserInterface.Input.text(getChaMActivity(), dialog_peer_textview_link.getText().toString(), "Enter Link", 5, 20,false , new ChaMCoreAPI.ChaMAPIInterface<String>() {
            @Override
            public void onResult(final String result) {
                ChaMCoreAPI.Interface.Protocol.peer_set(null, peer_id, "LINK", "", result, "SET", new ChaMCoreAPI.Interface.Protocol.ProtocolInterface() {
                    @Override
                    public void onRequest(String token, JSONObject request) {
                        dialog_peer_swiperefreshlayout.setRefreshing(true);
                    }

                    @Override
                    public void onError(String error) {
                        dialog_peer_swiperefreshlayout.setRefreshing(false);
                        ChaMAPI.UserInterface.Output.snack(getChaMView(),getChaMActivity(),dialog_peer_swiperefreshlayout,"Some error occurs !");
                    }

                    @Override
                    public void onResponse(JSONObject response) {
                        try{
                            dialog_peer_swiperefreshlayout.setRefreshing(false);
                            peer_infos.put("LINK",result);
                            load_infos();
                            ChaMAPI.UserInterface.Output.snack(getChaMView(),getChaMActivity(),dialog_peer_swiperefreshlayout,"Link sets !");
                        }catch (Exception ignored){}

                    }
                });
            }
        });
    }

    private void edit_name(){
        ChaMAPI.UserInterface.Input.text(getChaMActivity(), dialog_peer_textview_name.getText().toString(), "Enter Name", 0, 40, false, new ChaMCoreAPI.ChaMAPIInterface<String>() {
            @Override
            public void onResult(final String result) {
                ChaMCoreAPI.Interface.Protocol.peer_set(null, peer_id, "NAME", "", result, "SET", new ChaMCoreAPI.Interface.Protocol.ProtocolInterface() {
                    @Override
                    public void onRequest(String token, JSONObject request) {
                        dialog_peer_swiperefreshlayout.setRefreshing(true);
                    }

                    @Override
                    public void onError(String error) {
                        dialog_peer_swiperefreshlayout.setRefreshing(false);
                        ChaMAPI.UserInterface.Output.snack(getChaMView(),getChaMActivity(),dialog_peer_swiperefreshlayout,"Some error occurs !");
                    }

                    @Override
                    public void onResponse(JSONObject response) {
                        try{
                            dialog_peer_swiperefreshlayout.setRefreshing(false);
                            peer_infos.put("NAME",result);
                            load_infos();
                            ChaMAPI.UserInterface.Output.snack(getChaMView(),getChaMActivity(),dialog_peer_swiperefreshlayout,"Name sets !");
                        }catch (Exception ignored){}

                    }
                });
            }
        });
    }

    private void edit_bio(){
        ChaMAPI.UserInterface.Input.text(getChaMActivity(), dialog_peer_textview_bio.getText().toString(), "Enter Bio", 0, 40, false, new ChaMCoreAPI.ChaMAPIInterface<String>() {
            @Override
            public void onResult(final String result) {
                ChaMCoreAPI.Interface.Protocol.peer_set(null, peer_id, "BIO", "", result, "SET", new ChaMCoreAPI.Interface.Protocol.ProtocolInterface() {
                    @Override
                    public void onRequest(String token, JSONObject request) {
                        dialog_peer_swiperefreshlayout.setRefreshing(true);
                    }

                    @Override
                    public void onError(String error) {
                        dialog_peer_swiperefreshlayout.setRefreshing(false);
                        ChaMAPI.UserInterface.Output.snack(getChaMView(),getChaMActivity(),dialog_peer_swiperefreshlayout,"Some error occurs !");
                    }

                    @Override
                    public void onResponse(JSONObject response) {
                        try{
                            dialog_peer_swiperefreshlayout.setRefreshing(false);
                            peer_infos.put("BIO",result);
                            load_infos();
                            ChaMAPI.UserInterface.Output.snack(getChaMView(),getChaMActivity(),dialog_peer_swiperefreshlayout,"Bio sets !");
                        }catch (Exception ignored){}

                    }
                });
            }
        });
    }

}
