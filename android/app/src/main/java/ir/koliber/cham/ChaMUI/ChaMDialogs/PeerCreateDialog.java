package ir.koliber.cham.ChaMUI.ChaMDialogs;

import android.support.design.widget.TextInputLayout;
import android.support.v4.app.FragmentManager;
import android.support.v4.widget.SwipeRefreshLayout;
import android.util.Pair;
import android.view.View;
import org.json.JSONObject;
import java.util.ArrayList;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMActivity;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMDialog;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMFragment;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMHttp;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMButton;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMEditText;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMSwipeRefreshLayout;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMTextView;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMToolbar;
import ir.koliber.cham.ChaMAPI.ChaMAPI;
import ir.koliber.cham.ChaMUI.ChaMAdapters.CountriesAdapter;
import ir.koliber.cham.R;

public class PeerCreateDialog extends ChaMDialog{

    private static ChaMCoreAPI.ChaMAPIInterface<Pair<String,String>> api_interface;

    private ChaMToolbar dialog_peercreate_toolbar;

    public void open_dialog(ChaMActivity activity, ChaMCoreAPI.ChaMAPIInterface<Pair<String,String>> api_interface) {
        PeerCreateDialog.api_interface = api_interface;
        super.open_dialog(activity, R.layout.dialog_peercreate);
    }

    @Override
    protected void init_dialog() {
        init_variables();
        init_toolbar();
        init_content();
    }

    @Override
    protected void start_dialog() {
        PeerCreatePhoneFragment peercreatephone_fragment = new PeerCreatePhoneFragment();
        peercreatephone_fragment.open_fragment(getChaMActivity(),getChildFragmentManager(),R.id.dialog_peercreate_framelayout);
    }

    private void init_variables(){
        dialog_peercreate_toolbar = getChaMView().findViewById(R.id.dialog_peercreate_toolbar);
    }

    private void init_toolbar(){
        dialog_peercreate_toolbar.setNavigationIcon(getChaMActivity().getResources().getDrawable(R.drawable.ic_back));
        dialog_peercreate_toolbar.setNavigationOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                close_dialog();
            }
        });
    }

    private void init_content(){}

    public static class PeerCreatePhoneFragment extends ChaMFragment {

        private ArrayList<CountriesAdapter.CountriesAdapterValue> countries;

        private ChaMSwipeRefreshLayout fragment_peercreatephone_swiperefreshlayout;
        private ChaMTextView fragment_peercreatephone_textview_ccode;
        private TextInputLayout fragment_peercreatephone_textinputlayout_phone;
        private ChaMEditText fragment_peercreatephone_edittext_phone;
        private ChaMButton fragment_peercreatephone_button_country;
        private ChaMButton fragment_peercreatephone_button_send;

        public void open_fragment(ChaMActivity activity, FragmentManager fragment_manager, int container_id) {
            super.open_fragment(activity,fragment_manager,false,container_id,R.layout.fragment_peercreatephone);
        }

        @Override
        protected void init_fragment() {
            init_variables();
            init_content();
        }

        @Override
        protected void start_fragment() {
            load_content();
            select_country("1");
        }

        private void init_variables(){
            fragment_peercreatephone_swiperefreshlayout = getChaMView().findViewById(R.id.fragment_peercreatephone_swiperefreshlayout);
            fragment_peercreatephone_textview_ccode = getChaMView().findViewById(R.id.fragment_peercreatephone_textview_ccode);
            fragment_peercreatephone_textinputlayout_phone = getChaMView().findViewById(R.id.fragment_peercreatephone_textinputlayout_phone);
            fragment_peercreatephone_edittext_phone = getChaMView().findViewById(R.id.fragment_peercreatephone_edittext_phone);
            fragment_peercreatephone_button_country = getChaMView().findViewById(R.id.fragment_peercreatephone_button_country);
            fragment_peercreatephone_button_send = getChaMView().findViewById(R.id.fragment_peercreatephone_button_send);
            countries = ChaMAPI.UserInterface.Do.countries();
        }

        private void init_content(){
            fragment_peercreatephone_button_country.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    ChaMAPI.UserInterface.Items.countries(getChaMActivity(), new ChaMCoreAPI.ChaMAPIInterface<CountriesAdapter.CountriesAdapterValue>() {
                        @Override
                        public void onResult(CountriesAdapter.CountriesAdapterValue result) {
                            select_country(result.getCountryName());
                        }
                    });
                }
            });
            fragment_peercreatephone_button_send.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    if(!fragment_peercreatephone_textinputlayout_phone.isCounterEnabled() || (fragment_peercreatephone_textinputlayout_phone.isCounterEnabled() && fragment_peercreatephone_edittext_phone.getText().length() == fragment_peercreatephone_textinputlayout_phone.getCounterMaxLength())){
                        final String phone = fragment_peercreatephone_textview_ccode.getText().toString()+fragment_peercreatephone_edittext_phone.getText().toString();
                        ChaMCoreAPI.Interface.Protocol.peer_captcha(null, phone, "SMS", new ChaMCoreAPI.Interface.Protocol.ProtocolInterface() {
                            @Override
                            public void onRequest(String token, JSONObject request) {
                                fragment_peercreatephone_button_send.setEnabled(false);
                                fragment_peercreatephone_swiperefreshlayout.setRefreshing(true);
                            }

                            @Override
                            public void onError(String error) {
                                fragment_peercreatephone_button_send.setEnabled(true);
                                fragment_peercreatephone_swiperefreshlayout.setRefreshing(false);
                                ChaMAPI.UserInterface.Output.snack(fragment_peercreatephone_swiperefreshlayout,getChaMActivity(),getChaMActivity().getCurrentFocus(),"Some error occurs !");
                            }

                            @Override
                            public void onResponse(JSONObject response) {
                                fragment_peercreatephone_button_send.setEnabled(true);
                                fragment_peercreatephone_swiperefreshlayout.setRefreshing(false);
                                PeerCreateCodeFragment peercreatecode_fragment = new PeerCreateCodeFragment();
                                peercreatecode_fragment.open_fragment(getChaMActivity(),getChaMFragmentManager(),getChaMContainerId(),phone);
                            }
                        });
                    }else{
                        fragment_peercreatephone_edittext_phone.setError("Enter Correct Phone");
                    }
                }
            });
            fragment_peercreatephone_swiperefreshlayout.setOnRefreshListener(new SwipeRefreshLayout.OnRefreshListener() {
                @Override
                public void onRefresh() {
                    load_content();
                }
            });
        }

        private void load_content(){
            ChaMCoreAPI.Http.get(new ChaMHttp.ChaMHttpInterface() {
                @Override
                public void onRequest() {
                    fragment_peercreatephone_swiperefreshlayout.setRefreshing(true);
                }

                @Override
                public void onProgress(int progress) {}

                @Override
                public void onResponse(final String response) {
                    fragment_peercreatephone_swiperefreshlayout.setRefreshing(false);
                    try {
                        JSONObject result = new JSONObject(response);
                        select_country(result.getString("countryCode"));
                    }catch (Exception e){
                        ChaMAPI.UserInterface.Output.snack(fragment_peercreatephone_swiperefreshlayout,getChaMActivity(),getChaMActivity().getCurrentFocus(),"Some error occurs !");
                    }
                }
            },"http://ip-api.com/json");
        }

        private void select_country(String data){
            for(int cursor = 0 ; cursor < countries.size() ; cursor++){
                CountriesAdapter.CountriesAdapterValue item = countries.get(cursor);
                if(data.toLowerCase().equals(item.getCountryCode().toLowerCase()) || data.toLowerCase().equals(item.getCountryCCode().toLowerCase())){
                    fragment_peercreatephone_button_country.setText(item.getCountryName()+" ("+item.getCountryCode()+")");
                    fragment_peercreatephone_textview_ccode.setText("+"+item.getCountryCCode());
                    fragment_peercreatephone_edittext_phone.setText("");
                    if(item.getCountryNumber().length() > 0){
                        fragment_peercreatephone_textinputlayout_phone.setCounterMaxLength(item.getCountryNumber().replaceAll(" ","").length());
                        fragment_peercreatephone_textinputlayout_phone.setCounterEnabled(true);
                    }else{
                        fragment_peercreatephone_textinputlayout_phone.setCounterEnabled(false);
                    }
                    break;
                }
            }
        }

    }

    public static class PeerCreateCodeFragment extends ChaMFragment {

        private String peer_phone;
        private int tries;

        private ChaMSwipeRefreshLayout fragment_peercreatecode_swiperefreshlayout;
        private ChaMEditText fragment_peercreatecode_edittext_code;
        private ChaMButton fragment_peercreatecode_button_call;
        private ChaMButton fragment_peercreatecode_button_check;
        private ChaMTextView fragment_peercreatecode_textview_timer;

        public void open_fragment(ChaMActivity activity, FragmentManager fragment_manager, int container_id, String peer_phone) {
            this.peer_phone = peer_phone;
            super.open_fragment(activity,fragment_manager,true,container_id,R.layout.fragment_peercreatecode);
        }

        @Override
        protected void init_fragment() {
            init_variables();
            init_content();
        }

        @Override
        protected void start_fragment() {}

        private void init_variables(){
            fragment_peercreatecode_swiperefreshlayout = getChaMView().findViewById(R.id.fragment_peercreatecode_swiperefreshlayout);
            fragment_peercreatecode_edittext_code = getChaMView().findViewById(R.id.fragment_peercreatecode_edittext_code);
            fragment_peercreatecode_button_call = getChaMView().findViewById(R.id.fragment_peercreatecode_button_call);
            fragment_peercreatecode_button_check = getChaMView().findViewById(R.id.fragment_peercreatecode_button_check);
            fragment_peercreatecode_textview_timer = getChaMView().findViewById(R.id.fragment_peercreatecode_textview_timer);
            tries = 0;
        }

        private void init_content(){
            fragment_peercreatecode_button_check.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    if(fragment_peercreatecode_edittext_code.getText().toString().length() == 6){
                        create_peer();
                    }else{
                        fragment_peercreatecode_edittext_code.setError("Enter Correct Code !");
                    }
                }
            });
            fragment_peercreatecode_button_call.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    ChaMCoreAPI.Interface.Protocol.peer_captcha(null, peer_phone, "CALL", new ChaMCoreAPI.Interface.Protocol.ProtocolInterface() {
                        @Override
                        public void onRequest(String token, JSONObject request) {
                            fragment_peercreatecode_button_call.setEnabled(false);
                            fragment_peercreatecode_swiperefreshlayout.setRefreshing(true);
                        }

                        @Override
                        public void onError(String error) {
                            fragment_peercreatecode_button_call.setEnabled(true);
                            fragment_peercreatecode_swiperefreshlayout.setRefreshing(false);
                            ChaMAPI.UserInterface.Output.snack(fragment_peercreatecode_swiperefreshlayout,getChaMActivity(),getChaMActivity().getCurrentFocus(),"Some error occurs !");
                        }

                        @Override
                        public void onResponse(JSONObject response) {
                            fragment_peercreatecode_button_call.setVisibility(View.GONE);
                            fragment_peercreatecode_button_call.setEnabled(true);
                            fragment_peercreatecode_swiperefreshlayout.setRefreshing(false);
                        }
                    });
                }
            });
            fragment_peercreatecode_swiperefreshlayout.setEnabled(false);
        }

        private void init_timer(){
            new Thread(new Runnable() {
                @Override
                public void run() {
                    // 3 min
                    fragment_peercreatecode_textview_timer.setVisibility(View.VISIBLE);
                    int time = 300;
                    while(time-- > 0){
                        try {
                            fragment_peercreatecode_textview_timer.setText((time/60 < 10 ? "0"+time/60 : time/60)+":"+(time%60 < 10 ? "0"+time%60 : time%60));
                            Thread.sleep(1000);
                        } catch (Exception ignored) {}
                    }
                    fragment_peercreatecode_textview_timer.setVisibility(View.GONE);
                    tries = 0;
                    fragment_peercreatecode_button_call.setVisibility(View.VISIBLE);
                    fragment_peercreatecode_button_check.setEnabled(true);
                }
            }).start();
        }

        private void create_peer(){
            ChaMCoreAPI.Interface.Protocol.peer_create(null, peer_phone, fragment_peercreatecode_edittext_code.getText().toString(), new ChaMCoreAPI.Interface.Protocol.ProtocolInterface() {
                @Override
                public void onRequest(String token, JSONObject request) {
                    fragment_peercreatecode_button_check.setEnabled(false);
                    fragment_peercreatecode_swiperefreshlayout.setRefreshing(true);
                    tries++;
                }

                @Override
                public void onError(String error) {
                    if(tries < 5){
                        fragment_peercreatecode_button_check.setEnabled(true);
                    }else{
                        init_timer();
                    }
                    fragment_peercreatecode_swiperefreshlayout.setRefreshing(false);
                    ChaMAPI.UserInterface.Output.snack(fragment_peercreatecode_swiperefreshlayout,getChaMActivity(),getChaMActivity().getCurrentFocus(),"Some error occurs !");
                }

                @Override
                public void onResponse(JSONObject response) {
                    try{
                        fragment_peercreatecode_button_check.setEnabled(true);
                        fragment_peercreatecode_swiperefreshlayout.setRefreshing(false);
                        init_peer(response.getString("ID"),response.getString("SCODE"));
                    }catch (Exception ignored){}
                }
            });
        }

        private void init_peer(String id, String scode){
            if(api_interface != null){
                api_interface.onResult(new Pair<String, String>(id,scode));
            }
        }

    }

}