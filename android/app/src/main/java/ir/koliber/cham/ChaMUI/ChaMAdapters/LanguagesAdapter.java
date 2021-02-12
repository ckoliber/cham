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

public class LanguagesAdapter extends ChaMAdapter<LanguagesAdapter.LanguagesAdapterHolder,LanguagesAdapter.LanguagesAdapterValue>{

    private ChaMCoreAPI.ChaMAPIInterface<LanguagesAdapter.LanguagesAdapterValue> api_interface;

    public LanguagesAdapter(ChaMActivity activity, ChaMRecyclerView recycler_view, ChaMCoreAPI.ChaMAPIInterface<LanguagesAdapter.LanguagesAdapterValue> api_interface){
        super(activity,recycler_view,new ChaMAdapterSearcher<LanguagesAdapterValue>() {
            @Override
            public boolean search(String search, LanguagesAdapterValue languagesAdapterValue) {
                return languagesAdapterValue.getLanguageName().toLowerCase().contains(search.toLowerCase());
            }
        },R.layout.adapter_languages);
        this.api_interface = api_interface;
    }

    @Override
    protected LanguagesAdapterHolder create_holder(View view) {
        return new LanguagesAdapterHolder(view);
    }

    @Override
    protected void init_holder(LanguagesAdapterHolder holder, final ChaMAdapterItem<LanguagesAdapterValue> item) {
        holder.adapter_languages_textview_name.setText(item.getValue().getLanguageName());
        holder.adapter_languages_textview_name.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if(api_interface != null){
                    api_interface.onResult(item.getValue());
                }
            }
        });
        holder.adapter_languages_cardview.setOnClickListener(new View.OnClickListener() {
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
        for(LanguagesAdapterValue value : ChaMAPI.UserInterface.Do.languages()){
            load(load,value);
        }
        loader(false);
        end(load);
    }

    class LanguagesAdapterHolder extends RecyclerView.ViewHolder{

        private View view;
        private ChaMCardView adapter_languages_cardview;
        private ChaMTextView adapter_languages_textview_name;

        LanguagesAdapterHolder(View view) {
            super(view);
            this.view = view;
            this.adapter_languages_cardview = view.findViewById(R.id.adapter_languages_cardview);
            this.adapter_languages_textview_name = view.findViewById(R.id.adapter_languages_textview_name);
        }
    }

    public static class LanguagesAdapterValue{

        private String language_name;
        private String language_code;

        public LanguagesAdapterValue(String language_name, String language_code) {
            this.language_name = language_name;
            this.language_code = language_code;
        }

        public String getLanguageName() {
            return language_name;
        }
        public String getLanguageCode() {
            return language_code;
        }

    }

}
