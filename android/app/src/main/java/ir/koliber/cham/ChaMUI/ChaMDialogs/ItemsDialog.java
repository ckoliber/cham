package ir.koliber.cham.ChaMUI.ChaMDialogs;

import android.support.v7.widget.Toolbar;
import android.view.MenuItem;
import android.view.View;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMActivity;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMAdapter;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMDialog;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMEditText;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMRecyclerView;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMToolbar;
import ir.koliber.cham.ChaMAPI.ChaMAPI;
import ir.koliber.cham.ChaMUI.ChaMAdapters.ConnectionPeersAdapter;
import ir.koliber.cham.ChaMUI.ChaMAdapters.CountriesAdapter;
import ir.koliber.cham.ChaMUI.ChaMAdapters.LanguagesAdapter;
import ir.koliber.cham.ChaMUI.ChaMAdapters.MimesAdapter;
import ir.koliber.cham.ChaMUI.ChaMAdapters.PeerConnectionsAdapter;
import ir.koliber.cham.ChaMUI.ChaMAdapters.PeerPeersAdapter;
import ir.koliber.cham.ChaMUI.ChaMAdapters.StreamsAdapter;
import ir.koliber.cham.R;

public class ItemsDialog extends ChaMDialog{

    private ItemsDialogType type;
    private String id;
    private String self_permission;
    private int multi_select; // -1 -> non selectable , 0 -> one select , 1 -> multi select
    private ChaMCoreAPI.ChaMAPIInterface api_interface;
    private ChaMToolbar dialog_items_toolbar;
    private ChaMEditText dialog_items_edittext_search;
    private ChaMRecyclerView dialog_items_recyclerview;
    private ChaMAdapter adapter;

    public void open_dialog(ItemsDialogType type, ChaMActivity activity, String id, String self_permission){
        this.type = type;
        this.id = id;
        this.self_permission = self_permission;
        this.multi_select = -1;
        this.api_interface = null;
        super.open_dialog(activity, R.layout.dialog_items);
    }

    public void open_dialog(ItemsDialogType type, ChaMActivity activity, int multi_select, ChaMCoreAPI.ChaMAPIInterface api_interface) {
        this.type = type;
        this.id = null;
        this.self_permission = null;
        this.multi_select = multi_select;
        this.api_interface = api_interface;
        super.open_dialog(activity, R.layout.dialog_items);
    }

    public void open_dialog(ItemsDialogType type, ChaMActivity activity){
        this.type = type;
        this.id = null;
        this.self_permission = null;
        this.multi_select = -1;
        this.api_interface = null;
        super.open_dialog(activity, R.layout.dialog_items);
    }

    @Override
    protected void init_dialog() {
        init_variables();
        init_toolbar();
        init_content();
    }

    @Override
    protected void start_dialog() {
        adapter.loader(true);
    }

    private void init_variables(){
        dialog_items_toolbar = getChaMView().findViewById(R.id.dialog_items_toolbar);
        dialog_items_edittext_search = getChaMView().findViewById(R.id.dialog_items_edittext_search);
        dialog_items_recyclerview = getChaMView().findViewById(R.id.dialog_items_recyclerview);
    }

    private void init_toolbar(){
        switch (type){
            case ITEMS_DIALOG_COUNTRIES:
                dialog_items_toolbar.inflateMenu(R.menu.menu_search);
                break;
            case ITEMS_DIALOG_LANGUAGES:
                dialog_items_toolbar.inflateMenu(R.menu.menu_search);
                break;
            case ITEMS_DIALOG_STREAMS_SET:
                dialog_items_toolbar.inflateMenu(R.menu.menu_stream);
                break;
            case ITEMS_DIALOG_STREAMS_GET:
                dialog_items_toolbar.inflateMenu(R.menu.menu_stream);
                break;
            case ITEMS_DIALOG_MIMES_SET:
                dialog_items_toolbar.inflateMenu(R.menu.menu_search);
                break;
            case ITEMS_DIALOG_MIMES_GET:
                dialog_items_toolbar.inflateMenu(R.menu.menu_search);
                break;
            case ITEMS_DIALOG_PEER_CONNECTIONS:
                if(multi_select < 0){
                    dialog_items_toolbar.inflateMenu(R.menu.menu_search);
                }else if(multi_select == 0){
                    dialog_items_toolbar.inflateMenu(R.menu.menu_search);
                }else{
                    dialog_items_toolbar.inflateMenu(R.menu.menu_select);
                }
                break;
            case ITEMS_DIALOG_PEER_PEERS:
                dialog_items_toolbar.inflateMenu(R.menu.menu_search);
                break;
            case ITEMS_DIALOG_CONNECTION_PEERS:
                dialog_items_toolbar.inflateMenu(R.menu.menu_search);
                break;
        }
        dialog_items_toolbar.setNavigationIcon(getChaMActivity().getResources().getDrawable(R.drawable.ic_back));
        dialog_items_toolbar.setNavigationOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                close_dialog();
            }
        });
        dialog_items_toolbar.setOnMenuItemClickListener(new Toolbar.OnMenuItemClickListener() {
            @Override
            public boolean onMenuItemClick(MenuItem item) {
                switch (item.getItemId()){
                    case R.id.menu_search_search:
                        search_result();
                        break;
                    case R.id.menu_select_search:
                        search_result();
                        break;
                    case R.id.menu_select_check:
                        select_result();
                        break;
                    case R.id.menu_stream_search:
                        search_result();
                        break;
                    case R.id.menu_stream_auto:
                        auto_result();
                        break;
                }
                return false;
            }
        });
    }

    private void init_content(){
        switch (type){
            case ITEMS_DIALOG_COUNTRIES:
                adapter = new CountriesAdapter(getChaMActivity(), dialog_items_recyclerview, new ChaMCoreAPI.ChaMAPIInterface<CountriesAdapter.CountriesAdapterValue>() {
                    @Override
                    public void onResult(CountriesAdapter.CountriesAdapterValue result) {
                        api_interface.onResult(result);
                    }
                });
                break;
            case ITEMS_DIALOG_LANGUAGES:
                adapter = new LanguagesAdapter(getChaMActivity(), dialog_items_recyclerview, new ChaMCoreAPI.ChaMAPIInterface<LanguagesAdapter.LanguagesAdapterValue>() {
                    @Override
                    public void onResult(LanguagesAdapter.LanguagesAdapterValue result) {
                        api_interface.onResult(result);
                    }
                });
                break;
            case ITEMS_DIALOG_STREAMS_SET:
                adapter = new StreamsAdapter(getChaMActivity(),dialog_items_recyclerview, StreamsAdapter.StreamAdapterType.STREAM_ADAPTER_SET);
                break;
            case ITEMS_DIALOG_STREAMS_GET:
                adapter = new StreamsAdapter(getChaMActivity(),dialog_items_recyclerview, StreamsAdapter.StreamAdapterType.STREAM_ADAPTER_GET);
                break;
            case ITEMS_DIALOG_MIMES_SET:
                adapter = new MimesAdapter(getChaMActivity(),dialog_items_recyclerview, MimesAdapter.MimesAdapterType.MIME_ADAPTER_SET);
                break;
            case ITEMS_DIALOG_MIMES_GET:
                adapter = new MimesAdapter(getChaMActivity(),dialog_items_recyclerview, MimesAdapter.MimesAdapterType.MIME_ADAPTER_GET);
                break;
            case ITEMS_DIALOG_PEER_CONNECTIONS:
                if(multi_select < 0){
                    adapter = new PeerConnectionsAdapter(getChaMActivity(), dialog_items_recyclerview, id, self_permission, new ChaMCoreAPI.ChaMAPIInterface<String>() {
                        @Override
                        public void onResult(String result) {
                            ChaMAPI.UserInterface.Output.connection(getChaMActivity(),((PeerConnectionsAdapter.PeerConnectionsAdapterValue)adapter.get(result)).getConnectionID());
                        }
                    });
                }else if(multi_select == 0){
                    adapter = new PeerConnectionsAdapter(getChaMActivity(), dialog_items_recyclerview, id, self_permission, new ChaMCoreAPI.ChaMAPIInterface<String>() {
                        @Override
                        public void onResult(String result) {
                            adapter.select(result);
                            select_result();
                        }
                    });
                }else{
                    adapter = new PeerConnectionsAdapter(getChaMActivity(), dialog_items_recyclerview, id, self_permission, new ChaMCoreAPI.ChaMAPIInterface<String>() {
                        @Override
                        public void onResult(String result) {
                            adapter.select(result);
                        }
                    });
                }
                break;
            case ITEMS_DIALOG_PEER_PEERS:
                adapter = new PeerPeersAdapter(getChaMActivity(), dialog_items_recyclerview, id, self_permission, new ChaMCoreAPI.ChaMAPIInterface<String>() {
                    @Override
                    public void onResult(String result) {
                        ChaMAPI.UserInterface.Output.peer(getChaMActivity(),((PeerPeersAdapter.PeerPeersAdapterValue)adapter.get(result)).getPeerID());
                    }
                });
                break;
            case ITEMS_DIALOG_CONNECTION_PEERS:
                adapter = new ConnectionPeersAdapter(getChaMActivity(), dialog_items_recyclerview, id, self_permission, new ChaMCoreAPI.ChaMAPIInterface<String>() {
                    @Override
                    public void onResult(String result) {
                        ChaMAPI.UserInterface.Output.peer(getChaMActivity(),((ConnectionPeersAdapter.ConnectionPeersAdapterValue)adapter.get(result)).getPeerID());
                    }
                });
                break;
        }
        dialog_items_recyclerview.setAdapter(adapter);
    }

    private void search_result(){
        adapter.filter(dialog_items_edittext_search.getText().toString());
    }

    private void select_result(){

        if(api_interface != null){
            api_interface.onResult(adapter.selection());
        }
        close_dialog();
    }

    private void auto_result(){
        if(type == ItemsDialogType.ITEMS_DIALOG_STREAMS_SET){
            ChaMAPI.UserInterface.Items.mimes(getChaMActivity(),true);
        }else if(type == ItemsDialogType.ITEMS_DIALOG_STREAMS_GET){
            ChaMAPI.UserInterface.Items.mimes(getChaMActivity(),false);
        }
    }

    public enum ItemsDialogType{
        ITEMS_DIALOG_COUNTRIES,
        ITEMS_DIALOG_LANGUAGES,
        ITEMS_DIALOG_STREAMS_SET,
        ITEMS_DIALOG_STREAMS_GET,
        ITEMS_DIALOG_MIMES_SET,
        ITEMS_DIALOG_MIMES_GET,
        ITEMS_DIALOG_PEER_CONNECTIONS,
        ITEMS_DIALOG_PEER_PEERS,
        ITEMS_DIALOG_CONNECTION_PEERS
    }

}
