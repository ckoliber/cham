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

public class PeerConnectionsAdapter extends ChaMAdapter<PeerConnectionsAdapter.PeerConnectionsAdapterHolder,PeerConnectionsAdapter.PeerConnectionsAdapterValue>{

    private String peer_id;
    private ChaMAPI.UserInterface.Permission.PeerPermission self_permission;
    private ChaMCoreAPI.ChaMAPIInterface<String> api_interface;

    public PeerConnectionsAdapter(ChaMActivity activity, ChaMRecyclerView recycler_view, String peer_id, String self_permission, ChaMCoreAPI.ChaMAPIInterface<String> api_interface){
        super(activity,recycler_view,new ChaMAdapterSearcher<PeerConnectionsAdapterValue>() {
            @Override
            public boolean search(String search, PeerConnectionsAdapterValue peerConnectionsAdapterValue) {
                return peerConnectionsAdapterValue.getConnectionName().toLowerCase().contains(search.toLowerCase());
            }
        },R.layout.adapter_peerconnections);
        this.peer_id = peer_id;
        this.api_interface = api_interface;
        this.self_permission = ChaMAPI.UserInterface.Permission.peer(self_permission);
    }

    @Override
    protected PeerConnectionsAdapterHolder create_holder(View view) {
        return new PeerConnectionsAdapterHolder(view);
    }

    @Override
    protected void init_holder(PeerConnectionsAdapterHolder holder, final ChaMAdapterItem<PeerConnectionsAdapterValue> item) {
        holder.view.setBackgroundColor(item.getSelected() ? 0x993949ab : 0);
        holder.adapter_peerconnections_fab_remove.setVisibility((self_permission != null && (self_permission.getConnections() == 3 || self_permission.getConnections() == 4)) ? View.VISIBLE : View.GONE);
        holder.adapter_peerconnections_textview_name.setText(item.getValue().getConnectionName());
        // load picture
        holder.adapter_peerconnections_cardview.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if(api_interface != null){
                    api_interface.onResult(item.getKey());
                }
            }
        });
        holder.adapter_peerconnections_imageview_picture.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Output.pictures(peer_id, ChaMAPI.UserInterface.TargetType.TARGET_TYPE_PEER);
            }
        });
        holder.adapter_peerconnections_textview_name.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if(api_interface != null){
                    api_interface.onResult(item.getKey());
                }
            }
        });
        holder.adapter_peerconnections_fab_remove.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.confirm(getChaMActivity(), "Remove connection ?", new ChaMCoreAPI.ChaMAPIInterface<Boolean>() {
                    @Override
                    public void onResult(Boolean result) {
                        if(result){
                            ChaMCoreAPI.Interface.Protocol.peer_set(null,peer_id,"CONNECTIONS",item.getValue().getConnectionID(),"","DELETE",new ChaMCoreAPI.Interface.Protocol.ProtocolInterface() {
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
                                    ChaMAPI.UserInterface.Output.snack(getChaMRecyclerView(),getChaMActivity(),getChaMRecyclerView(),"Connection removed !");
                                    remove(item.getKey());
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
        ChaMCoreAPI.Interface.Protocol.peer_load(null,peer_id,"PEER_CONNECTIONS","","","",0,1,new ChaMCoreAPI.Interface.Protocol.ProtocolInterface() {
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
                    JSONObject peer_connections = new JSONObject(response.getString("VALUE"));
                    final AtomicInteger peer_loads = new AtomicInteger(peer_connections.length());
                    Iterator<String> iterator = peer_connections.keys();
                    while(iterator.hasNext()){
                        final String connection_id = iterator.next();
                        ChaMCoreAPI.Interface.Protocol.peer_load(null,connection_id,"CONNECTION_INFOS","","","",1,1,new ChaMCoreAPI.Interface.Protocol.ProtocolInterface() {
                            @Override
                            public void onRequest(String token, JSONObject request) {}

                            @Override
                            public void onError(String error) {
                                if(peer_loads.decrementAndGet() == 0){
                                    loader(false);
                                    end(load);
                                }
                            }

                            @Override
                            public void onResponse(JSONObject response) {
                                try{
                                    if(peer_loads.decrementAndGet() == 0){
                                        loader(false);
                                        end(load);
                                    }
                                    JSONObject connection_object = new JSONObject(response.getString("VALUE"));
                                    load(load,new PeerConnectionsAdapterValue(connection_id,connection_object.getString("NAME"),connection_object.getString("PICTURE")));
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

    class PeerConnectionsAdapterHolder extends RecyclerView.ViewHolder{

        private View view;
        private ChaMCardView adapter_peerconnections_cardview;
        private ChaMImageView adapter_peerconnections_imageview_picture;
        private ChaMTextView adapter_peerconnections_textview_name;
        private ChaMFab adapter_peerconnections_fab_remove;

        PeerConnectionsAdapterHolder(View view) {
            super(view);
            this.view = view;
            this.adapter_peerconnections_cardview = view.findViewById(R.id.adapter_peerconnections_cardview);
            this.adapter_peerconnections_imageview_picture = view.findViewById(R.id.adapter_peerconnections_imageview_picture);
            this.adapter_peerconnections_textview_name = view.findViewById(R.id.adapter_peerconnections_textview_name);
            this.adapter_peerconnections_fab_remove = view.findViewById(R.id.adapter_peerconnections_fab_remove);
        }
    }

    public static class PeerConnectionsAdapterValue {

        private String connection_id;
        private String connection_name;
        private String connection_picture;

        public PeerConnectionsAdapterValue(String connection_id, String connection_name, String connection_picture){
            this.connection_id = connection_id;
            this.connection_name = connection_name;
            this.connection_picture = connection_picture;
        }

        public String getConnectionID(){
            return connection_id;
        }
        public String getConnectionName(){
            return connection_name;
        }
        public String getConnectionPicture(){
            return connection_picture;
        }
    }

}
