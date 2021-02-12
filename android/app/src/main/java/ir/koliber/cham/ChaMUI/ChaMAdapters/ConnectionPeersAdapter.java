package ir.koliber.cham.ChaMUI.ChaMAdapters;

import android.support.v7.widget.RecyclerView;
import android.view.View;
import org.json.JSONObject;
import java.util.Iterator;
import java.util.concurrent.atomic.AtomicInteger;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMActivity;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMAdapter;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMCardView;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMFab;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMImageView;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMRecyclerView;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMTextView;
import ir.koliber.cham.ChaMAPI.ChaMAPI;
import ir.koliber.cham.R;

public class ConnectionPeersAdapter extends ChaMAdapter<ConnectionPeersAdapter.ConnectionPeersAdapterHolder,ConnectionPeersAdapter.ConnectionPeersAdapterValue>{

    // specific permissions :
    //      _ : all peers
    //      * : join

    private String connection_id;
    private ChaMAPI.UserInterface.Permission.ConnectionPermission self_permission;
    private ChaMCoreAPI.ChaMAPIInterface<String> api_interface;

    public ConnectionPeersAdapter(ChaMActivity activity, ChaMRecyclerView recycler_view, String connection_id, String self_permission, ChaMCoreAPI.ChaMAPIInterface<String> api_interface){
        super(activity,recycler_view,new ChaMAdapterSearcher<ConnectionPeersAdapter.ConnectionPeersAdapterValue>() {
            @Override
            public boolean search(String search, ConnectionPeersAdapter.ConnectionPeersAdapterValue connectionsAdapterValue) {
                return connectionsAdapterValue.getPeerName().toLowerCase().contains(search.toLowerCase());
            }
        },R.layout.adapter_connectionpeers);
        this.connection_id = connection_id;
        this.api_interface = api_interface;
        this.self_permission = ChaMAPI.UserInterface.Permission.connection(self_permission);
    }

    @Override
    protected ConnectionPeersAdapterHolder create_holder(View view) {
        return new ConnectionPeersAdapterHolder(view);
    }

    @Override
    protected void init_holder(ConnectionPeersAdapterHolder holder, final ChaMAdapterItem<ConnectionPeersAdapter.ConnectionPeersAdapterValue> item) {
        holder.view.setBackgroundColor(item.getSelected() ? 0x993949ab : 0);
        holder.adapter_connectionpeers_fab_remove.setVisibility((!item.getValue().getPeerID().equals("_") && !item.getValue().getPeerID().equals("*")) && (self_permission.getPeers() == 4) || (self_permission.getPeers() == 3 && item.getValue().getPeerPermission().getPeers() <= 1) ? View.VISIBLE : View.GONE);
        holder.adapter_connectionpeers_textview_name.setText(item.getValue().getPeerName());
        holder.adapter_connectionpeers_imageview_permission.setImageDrawable(item.getValue().getPeerPermission().getDrawable());
        // load picture
        holder.adapter_connectionpeers_cardview.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if(api_interface != null){
                    api_interface.onResult(item.getKey());
                }
            }
        });
        holder.adapter_connectionpeers_imageview_picture.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Output.pictures(connection_id, ChaMAPI.UserInterface.TargetType.TARGET_TYPE_CONNECTION);
            }
        });
        holder.adapter_connectionpeers_textview_name.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if(api_interface != null){
                    api_interface.onResult(item.getKey());
                }
            }
        });
        holder.adapter_connectionpeers_fab_remove.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.confirm(getChaMActivity(), "Remove peer ?", new ChaMCoreAPI.ChaMAPIInterface<Boolean>() {
                    @Override
                    public void onResult(Boolean result) {
                        if(result){
                            ChaMCoreAPI.Interface.Protocol.connection_set(null,connection_id,"PEERS",item.getValue().getPeerID(),"","DELETE",new ChaMCoreAPI.Interface.Protocol.ProtocolInterface() {
                                @Override
                                public void onRequest(String token, JSONObject request) {
                                    getChaMRecyclerView().setRefreshing(true);
                                }

                                @Override
                                public void onError(String error) {
                                    getChaMRecyclerView().setRefreshing(false);
                                    ChaMAPI.UserInterface.Output.snack(getChaMRecyclerView(),getChaMActivity(),getChaMRecyclerView(),"Some error occurs !");
                                }

                                @Override
                                public void onResponse(JSONObject response) {
                                    getChaMRecyclerView().setRefreshing(false);
                                    ChaMAPI.UserInterface.Output.snack(getChaMRecyclerView(),getChaMActivity(),getChaMRecyclerView(),"Peer removed !");
                                    remove(item.getKey());
                                }
                            });
                        }
                    }
                });
            }
        });
        holder.adapter_connectionpeers_imageview_permission.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Output.permission(item.getValue().getPeerPermission().toString(), self_permission.getPeers() == 3 || self_permission.getPeers() == 4, new ChaMCoreAPI.ChaMAPIInterface<String>() {
                    @Override
                    public void onResult(final String result) {
                        if(!item.getValue().getPeerPermission().toString().equals(result)){
                            ChaMCoreAPI.Interface.Protocol.connection_set(null,connection_id,"PEERS",item.getValue().getPeerID(),result,"ADD",new ChaMCoreAPI.Interface.Protocol.ProtocolInterface() {
                                @Override
                                public void onRequest(String token, JSONObject request) {
                                    getChaMRecyclerView().setRefreshing(true);
                                }

                                @Override
                                public void onError(String error) {
                                    getChaMRecyclerView().setRefreshing(false);
                                    ChaMAPI.UserInterface.Output.snack(getChaMRecyclerView(),getChaMActivity(),getChaMRecyclerView(),"Some error occurs !");
                                }

                                @Override
                                public void onResponse(JSONObject response) {
                                    getChaMRecyclerView().setRefreshing(false);
                                    ChaMAPI.UserInterface.Output.snack(getChaMRecyclerView(),getChaMActivity(),getChaMRecyclerView(),"Peer added !");
                                    item.getValue().setPeerPermission(result);
                                    change(item.getKey());
                                }
                            });
                        }
                    }
                });
            }
        });
    }

    @Override
    protected void load_holder(final ChaMAdapterLoad load) {
        ChaMCoreAPI.Interface.Protocol.peer_load(null,connection_id,"CONNECTION_PEERS","","","", 0, 1,new ChaMCoreAPI.Interface.Protocol.ProtocolInterface() {
            @Override
            public void onRequest(String token, JSONObject request) {}

            @Override
            public void onError(String error) {
                loader(false);
                end(load);
                ChaMAPI.UserInterface.Output.snack(getChaMRecyclerView(),getChaMActivity(),getChaMRecyclerView(),"Some error occurs !");
            }

            @Override
            public void onResponse(JSONObject response) {
                try{
                    JSONObject connection_peers = new JSONObject(response.getString("VALUE"));
                    final AtomicInteger connection_loads = new AtomicInteger(connection_peers.length());
                    Iterator<String> iterator = connection_peers.keys();
                    while(iterator.hasNext()){
                        final String peer_id = iterator.next();
                        final String peer_permission = connection_peers.getString(peer_id);
                        ChaMCoreAPI.Interface.Protocol.peer_load(null, peer_id, "PEER_INFOS", "", "", "", 1, 1, new ChaMCoreAPI.Interface.Protocol.ProtocolInterface() {
                            @Override
                            public void onRequest(String token, JSONObject request) {}

                            @Override
                            public void onError(String error) {
                                if(connection_loads.decrementAndGet() == 0){
                                    loader(false);
                                    end(load);
                                }
                            }

                            @Override
                            public void onResponse(JSONObject response) {
                                try{
                                    if(connection_loads.decrementAndGet() == 0){
                                        loader(false);
                                        end(load);
                                    }
                                    JSONObject peer_object = new JSONObject(response.getString("VALUE"));
                                    load(load,new ConnectionPeersAdapterValue(peer_id,peer_object.getString("NAME"),peer_object.getString("PICTURE"),peer_permission));
                                }catch (Exception ignored){}
                            }
                        });
                    }
                }catch (Exception ignored){
                    loader(false);
                    end(load);
                    ChaMAPI.UserInterface.Output.snack(getChaMRecyclerView(),getChaMActivity(),getChaMRecyclerView(),"Some error occurs !");
                }
            }
        });
    }

    class ConnectionPeersAdapterHolder extends RecyclerView.ViewHolder{

        private View view;
        private ChaMCardView adapter_connectionpeers_cardview;
        private ChaMImageView adapter_connectionpeers_imageview_picture;
        private ChaMTextView adapter_connectionpeers_textview_name;
        private ChaMFab adapter_connectionpeers_fab_remove;
        private ChaMImageView adapter_connectionpeers_imageview_permission;

        ConnectionPeersAdapterHolder(View view) {
            super(view);
            this.view = view;
            this.adapter_connectionpeers_cardview = view.findViewById(R.id.adapter_connectionpeers_cardview);
            this.adapter_connectionpeers_imageview_picture = view.findViewById(R.id.adapter_connectionpeers_imageview_picture);
            this.adapter_connectionpeers_textview_name = view.findViewById(R.id.adapter_connectionpeers_textview_name);
            this.adapter_connectionpeers_fab_remove = view.findViewById(R.id.adapter_connectionpeers_fab_remove);
            this.adapter_connectionpeers_imageview_permission = view.findViewById(R.id.adapter_connectionpeers_imageview_permission);
        }
    }

    public static class ConnectionPeersAdapterValue {

        private String peer_id;
        private String peer_name;
        private String peer_picture;
        private ChaMAPI.UserInterface.Permission.ConnectionPermission peer_permission;

        public ConnectionPeersAdapterValue(String peer_id, String peer_name, String peer_picture, String peer_permission){
            this.peer_id = peer_id;
            this.peer_name = peer_name;
            this.peer_picture = peer_picture;
            this.peer_permission = ChaMAPI.UserInterface.Permission.connection(peer_permission);
        }

        public String getPeerID(){
            return peer_id;
        }
        public String getPeerName(){
            return peer_name;
        }
        public String getPeerPicture(){
            return peer_picture;
        }
        public ChaMAPI.UserInterface.Permission.ConnectionPermission getPeerPermission(){
            return peer_permission;
        }
        public void setPeerPermission(String peer_permission){
            this.peer_permission = ChaMAPI.UserInterface.Permission.connection(peer_permission);
        }
    }

}
