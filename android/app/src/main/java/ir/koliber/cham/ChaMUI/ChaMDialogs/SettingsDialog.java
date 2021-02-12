package ir.koliber.cham.ChaMUI.ChaMDialogs;

import android.view.View;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMActivity;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMDialog;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMButton;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMCardView;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMTextView;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMToolbar;
import ir.koliber.cham.ChaMAPI.ChaMAPI;
import ir.koliber.cham.ChaMUI.ChaMActivities.StartActivity;
import ir.koliber.cham.ChaMUI.ChaMAdapters.LanguagesAdapter;
import ir.koliber.cham.R;

public class SettingsDialog extends ChaMDialog{

    private ChaMToolbar dialog_settings_toolbar;
    private ChaMCardView dialog_settings_cardview_language;
    private ChaMTextView dialog_settings_textview_language;
    private ChaMCardView dialog_settings_cardview_theme;
    private ChaMCardView dialog_settings_cardview_cache;
    private ChaMTextView dialog_settings_textview_cache;
    private ChaMCardView dialog_settings_cardview_pin;
    private ChaMButton dialog_settings_button_pin;

    public void open_dialog(ChaMActivity activity) {
        super.open_dialog(activity, R.layout.dialog_settings);
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
        dialog_settings_toolbar = getChaMView().findViewById(R.id.dialog_settings_toolbar);
        dialog_settings_cardview_language = getChaMView().findViewById(R.id.dialog_settings_cardview_language);
        dialog_settings_textview_language = getChaMView().findViewById(R.id.dialog_settings_textview_language);
        dialog_settings_cardview_theme = getChaMView().findViewById(R.id.dialog_settings_cardview_theme);
        dialog_settings_cardview_cache = getChaMView().findViewById(R.id.dialog_settings_cardview_cache);
        dialog_settings_textview_cache = getChaMView().findViewById(R.id.dialog_settings_textview_cache);
        dialog_settings_cardview_pin = getChaMView().findViewById(R.id.dialog_settings_cardview_pin);
        dialog_settings_button_pin = getChaMView().findViewById(R.id.dialog_settings_button_pin);
    }

    private void init_toolbar(){
        dialog_settings_toolbar.setNavigationIcon(getChaMActivity().getResources().getDrawable(R.drawable.ic_back));
        dialog_settings_toolbar.setNavigationOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                close_dialog();
            }
        });
    }

    private void init_content(){
        dialog_settings_cardview_language.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                click_language();
            }
        });
        dialog_settings_textview_language.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                click_language();
            }
        });
        dialog_settings_cardview_theme.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                click_theme();
            }
        });
        dialog_settings_cardview_cache.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                click_cache();
            }
        });
        dialog_settings_textview_cache.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                click_cache();
            }
        });
        dialog_settings_cardview_pin.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                click_pin();
            }
        });
        dialog_settings_button_pin.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                click_pin();
            }
        });
    }

    private void click_language(){
        ChaMAPI.UserInterface.Items.language(getChaMActivity(), new ChaMCoreAPI.ChaMAPIInterface<LanguagesAdapter.LanguagesAdapterValue>() {
            @Override
            public void onResult(LanguagesAdapter.LanguagesAdapterValue result) {
                ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.UI_LANGUAGE,result.getLanguageCode());
                ChaMAPI.UserInterface.Do.restart(getChaMActivity(), StartActivity.class);
            }
        });
    }

    private void click_theme(){
        ChaMAPI.UserInterface.Output.theme(getChaMActivity(),null);
    }

    private void click_cache(){
        ChaMCoreAPI.Interface.Stream.clear();
        load_content();
    }

    private void click_pin(){
        if(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.UI_PIN).length() > 0){
            ChaMAPI.UserInterface.Input.text(getChaMActivity(), "", "Your pin", 0, 4, true, new ChaMCoreAPI.ChaMAPIInterface<String>() {
                @Override
                public void onResult(String result) {
                    close_dialog();
                    if(result.equals(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.UI_PIN))){
                        init_pin();
                    }else{
                        click_pin();
                    }
                }
            });
        }else{
            init_pin();
        }
    }

    private void init_pin(){
        ChaMAPI.UserInterface.Input.text(getChaMActivity(), "", "New pin", 0, 4, true, new ChaMCoreAPI.ChaMAPIInterface<String>() {
            @Override
            public void onResult(String result) {
                ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.UI_PIN,result);
            }
        });
    }

    private void load_content(){
        for(LanguagesAdapter.LanguagesAdapterValue language : ChaMAPI.UserInterface.Do.languages()){
            if(language.getLanguageCode().equals(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.UI_LANGUAGE))){
                dialog_settings_textview_language.setText(language.getLanguageName());
                break;
            }
        }
        dialog_settings_textview_cache.setText(ChaMAPI.Text.size(ChaMCoreAPI.Interface.Stream.size()));
    }
}
