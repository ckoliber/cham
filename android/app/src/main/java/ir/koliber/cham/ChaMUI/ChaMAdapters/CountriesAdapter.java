package ir.koliber.cham.ChaMUI.ChaMAdapters;

import android.support.v7.widget.RecyclerView;
import android.view.View;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMActivity;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMAdapter;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMCardView;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMRecyclerView;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMTextView;
import ir.koliber.cham.ChaMAPI.ChaMAPI;
import ir.koliber.cham.R;

public class CountriesAdapter extends ChaMAdapter<CountriesAdapter.CountriesAdapterHolder,CountriesAdapter.CountriesAdapterValue>{

    private ChaMCoreAPI.ChaMAPIInterface<CountriesAdapterValue> api_interface;

    public CountriesAdapter(ChaMActivity activity, ChaMRecyclerView recycler_view, ChaMCoreAPI.ChaMAPIInterface<CountriesAdapterValue> api_interface){
        super(activity,recycler_view,new ChaMAdapterSearcher<CountriesAdapterValue>() {
            @Override
            public boolean search(String search, CountriesAdapterValue countriesAdapterValue) {
                return countriesAdapterValue.getCountryName().toLowerCase().contains(search.toLowerCase());
            }
        },R.layout.adapter_countries);
        this.api_interface = api_interface;
    }

    @Override
    protected CountriesAdapterHolder create_holder(View view) {
        return new CountriesAdapterHolder(view);
    }

    @Override
    protected void init_holder(CountriesAdapterHolder holder, final ChaMAdapterItem<CountriesAdapterValue> item) {
        holder.adapter_countries_textview_name.setText(item.getValue().getCountryName());
        holder.adapter_countries_textview_ccode.setText(item.getValue().getCountryCCode());
        holder.adapter_countries_textview_name.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if(api_interface != null){
                    api_interface.onResult(item.getValue());
                }
            }
        });
        holder.adapter_countries_textview_ccode.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if(api_interface != null){
                    api_interface.onResult(item.getValue());
                }
            }
        });
        holder.adapter_countries_cardview.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if(api_interface != null){
                    api_interface.onResult(item.getValue());
                }
            }
        });
    }

    @Override
    protected void load_holder(ChaMAdapterLoad load) {
        for(CountriesAdapterValue value : ChaMAPI.UserInterface.Do.countries()){
            load(load,value);
        }
        loader(false);
        end(load);
    }

    class CountriesAdapterHolder extends RecyclerView.ViewHolder{

        private View view;
        private ChaMCardView adapter_countries_cardview;
        private ChaMTextView adapter_countries_textview_ccode;
        private ChaMTextView adapter_countries_textview_name;

        CountriesAdapterHolder(View view) {
            super(view);
            this.view = view;
            this.adapter_countries_cardview = view.findViewById(R.id.adapter_countries_cardview);
            this.adapter_countries_textview_ccode = view.findViewById(R.id.adapter_countries_textview_ccode);
            this.adapter_countries_textview_name = view.findViewById(R.id.adapter_countries_textview_name);
        }
    }

    public static class CountriesAdapterValue {

        private String country_name;
        private String country_code;
        private String country_ccode;
        private String country_number;

        public CountriesAdapterValue(String country_name, String country_code, String country_ccode, String country_number) {
            this.country_name = country_name;
            this.country_code = country_code;
            this.country_ccode = country_ccode;
            this.country_number = country_number;
        }

        public String getCountryName() {
            return country_name;
        }
        public String getCountryCode() {
            return country_code;
        }
        public String getCountryCCode() {
            return country_ccode;
        }
        public String getCountryNumber() {
            return country_number;
        }

    }

}
