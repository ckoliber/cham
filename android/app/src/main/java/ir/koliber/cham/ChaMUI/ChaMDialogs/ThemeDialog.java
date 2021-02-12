package ir.koliber.cham.ChaMUI.ChaMDialogs;

import android.graphics.drawable.ColorDrawable;
import android.support.v7.widget.Toolbar;
import android.view.MenuItem;
import android.view.View;
import org.json.JSONObject;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMActivity;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMDialog;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMCardView;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMImageView;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMTextView;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMToolbar;
import ir.koliber.cham.ChaMAPI.ChaMAPI;
import ir.koliber.cham.ChaMUI.ChaMActivities.StartActivity;
import ir.koliber.cham.R;

public class ThemeDialog extends ChaMDialog{

    private String link;

    private ChaMToolbar dialog_theme_toolbar;
    private ChaMCardView dialog_theme_cardview_mainback;
    private ChaMImageView dialog_theme_imageview_mainback;
    private ChaMCardView dialog_theme_cardview_mainitem;
    private ChaMImageView dialog_theme_imageview_mainitem;
    private ChaMCardView dialog_theme_cardview_toolbarback;
    private ChaMImageView dialog_theme_imageview_toolbarback;
    private ChaMCardView dialog_theme_cardview_toolbaritem;
    private ChaMImageView dialog_theme_imageview_toolbaritem;
    private ChaMCardView dialog_theme_cardview_buttonmainback;
    private ChaMImageView dialog_theme_imageview_buttonmainback;
    private ChaMCardView dialog_theme_cardview_buttonmainitem;
    private ChaMImageView dialog_theme_imageview_buttonmainitem;
    private ChaMCardView dialog_theme_cardview_buttonsendback;
    private ChaMImageView dialog_theme_imageview_buttonsendback;
    private ChaMCardView dialog_theme_cardview_buttonsenditem;
    private ChaMImageView dialog_theme_imageview_buttonsenditem;
    private ChaMCardView dialog_theme_cardview_cardmainback;
    private ChaMImageView dialog_theme_imageview_cardmainback;
    private ChaMCardView dialog_theme_cardview_cardmainitem;
    private ChaMImageView dialog_theme_imageview_cardmainitem;
    private ChaMCardView dialog_theme_cardview_cardsendback;
    private ChaMImageView dialog_theme_imageview_cardsendback;
    private ChaMCardView dialog_theme_cardview_cardsenditem;
    private ChaMImageView dialog_theme_imageview_cardsenditem;
    private ChaMCardView dialog_theme_cardview_cardchatmeback;
    private ChaMImageView dialog_theme_imageview_cardchatmeback;
    private ChaMCardView dialog_theme_cardview_cardchatmeitem;
    private ChaMImageView dialog_theme_imageview_cardchatmeitem;
    private ChaMCardView dialog_theme_cardview_cardchatyouback;
    private ChaMImageView dialog_theme_imageview_cardchatyouback;
    private ChaMCardView dialog_theme_cardview_cardchatyouitem;
    private ChaMImageView dialog_theme_imageview_cardchatyouitem;
    private ChaMCardView dialog_theme_cardview_edittextmainback;
    private ChaMImageView dialog_theme_imageview_edittextmainback;
    private ChaMCardView dialog_theme_cardview_edittextmainitem;
    private ChaMImageView dialog_theme_imageview_edittextmainitem;
    private ChaMCardView dialog_theme_cardview_edittextsendback;
    private ChaMImageView dialog_theme_imageview_edittextsendback;
    private ChaMCardView dialog_theme_cardview_edittextsenditem;
    private ChaMImageView dialog_theme_imageview_edittextsenditem;
    private ChaMCardView dialog_theme_cardview_edittexttoolbarback;
    private ChaMImageView dialog_theme_imageview_edittexttoolbarback;
    private ChaMCardView dialog_theme_cardview_edittexttoolbaritem;
    private ChaMImageView dialog_theme_imageview_edittexttoolbaritem;
    private ChaMCardView dialog_theme_cardview_windownavbarback;
    private ChaMImageView dialog_theme_imageview_windownavbarback;
    private ChaMCardView dialog_theme_cardview_windowstatusbarback;
    private ChaMImageView dialog_theme_imageview_windowstatusbarback;
    private ChaMCardView dialog_theme_cardview_windowchatimage;
    private ChaMImageView dialog_theme_imageview_windowchatimage;
    private ChaMCardView dialog_theme_cardview_windowchatfont;
    private ChaMTextView dialog_theme_textview_windowchatfont;

    public void open_dialog(ChaMActivity activity, String link) {
        this.link = link;
        super.open_dialog(activity, R.layout.dialog_theme);
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
        dialog_theme_toolbar = getChaMView().findViewById(R.id.dialog_theme_toolbar);
        dialog_theme_cardview_mainback = getChaMView().findViewById(R.id.dialog_theme_cardview_mainback);
        dialog_theme_imageview_mainback = getChaMView().findViewById(R.id.dialog_theme_imageview_mainback);
        dialog_theme_cardview_mainitem = getChaMView().findViewById(R.id.dialog_theme_cardview_mainitem);
        dialog_theme_imageview_mainitem = getChaMView().findViewById(R.id.dialog_theme_imageview_mainitem);
        dialog_theme_cardview_toolbarback = getChaMView().findViewById(R.id.dialog_theme_cardview_toolbarback);
        dialog_theme_imageview_toolbarback = getChaMView().findViewById(R.id.dialog_theme_imageview_toolbarback);
        dialog_theme_cardview_toolbaritem = getChaMView().findViewById(R.id.dialog_theme_cardview_toolbaritem);
        dialog_theme_imageview_toolbaritem = getChaMView().findViewById(R.id.dialog_theme_imageview_toolbaritem);
        dialog_theme_cardview_buttonmainback = getChaMView().findViewById(R.id.dialog_theme_cardview_buttonmainback);
        dialog_theme_imageview_buttonmainback = getChaMView().findViewById(R.id.dialog_theme_imageview_buttonmainback);
        dialog_theme_cardview_buttonmainitem = getChaMView().findViewById(R.id.dialog_theme_cardview_buttonmainitem);
        dialog_theme_imageview_buttonmainitem = getChaMView().findViewById(R.id.dialog_theme_imageview_buttonmainitem);
        dialog_theme_cardview_buttonsendback = getChaMView().findViewById(R.id.dialog_theme_cardview_buttonsendback);
        dialog_theme_imageview_buttonsendback = getChaMView().findViewById(R.id.dialog_theme_imageview_buttonsendback);
        dialog_theme_cardview_buttonsenditem = getChaMView().findViewById(R.id.dialog_theme_cardview_buttonsenditem);
        dialog_theme_imageview_buttonsenditem = getChaMView().findViewById(R.id.dialog_theme_imageview_buttonsenditem);
        dialog_theme_cardview_cardmainback = getChaMView().findViewById(R.id.dialog_theme_cardview_cardmainback);
        dialog_theme_imageview_cardmainback = getChaMView().findViewById(R.id.dialog_theme_imageview_cardmainback);
        dialog_theme_cardview_cardmainitem = getChaMView().findViewById(R.id.dialog_theme_cardview_cardmainitem);
        dialog_theme_imageview_cardmainitem = getChaMView().findViewById(R.id.dialog_theme_imageview_cardmainitem);
        dialog_theme_cardview_cardsendback = getChaMView().findViewById(R.id.dialog_theme_cardview_cardsendback);
        dialog_theme_imageview_cardsendback = getChaMView().findViewById(R.id.dialog_theme_imageview_cardsendback);
        dialog_theme_cardview_cardsenditem = getChaMView().findViewById(R.id.dialog_theme_cardview_cardsenditem);
        dialog_theme_imageview_cardsenditem = getChaMView().findViewById(R.id.dialog_theme_imageview_cardsenditem);
        dialog_theme_cardview_cardchatmeback = getChaMView().findViewById(R.id.dialog_theme_cardview_cardchatmeback);
        dialog_theme_imageview_cardchatmeback = getChaMView().findViewById(R.id.dialog_theme_imageview_cardchatmeback);
        dialog_theme_cardview_cardchatmeitem = getChaMView().findViewById(R.id.dialog_theme_cardview_cardchatmeitem);
        dialog_theme_imageview_cardchatmeitem = getChaMView().findViewById(R.id.dialog_theme_imageview_cardchatmeitem);
        dialog_theme_cardview_cardchatyouback = getChaMView().findViewById(R.id.dialog_theme_cardview_cardchatyouback);
        dialog_theme_imageview_cardchatyouback = getChaMView().findViewById(R.id.dialog_theme_imageview_cardchatyouback);
        dialog_theme_cardview_cardchatyouitem = getChaMView().findViewById(R.id.dialog_theme_cardview_cardchatyouitem);
        dialog_theme_imageview_cardchatyouitem = getChaMView().findViewById(R.id.dialog_theme_imageview_cardchatyouitem);
        dialog_theme_cardview_edittextmainback = getChaMView().findViewById(R.id.dialog_theme_cardview_edittextmainback);
        dialog_theme_imageview_edittextmainback = getChaMView().findViewById(R.id.dialog_theme_imageview_edittextmainback);
        dialog_theme_cardview_edittextmainitem = getChaMView().findViewById(R.id.dialog_theme_cardview_edittextmainitem);
        dialog_theme_imageview_edittextmainitem = getChaMView().findViewById(R.id.dialog_theme_imageview_edittextmainitem);
        dialog_theme_cardview_edittextsendback = getChaMView().findViewById(R.id.dialog_theme_cardview_edittextsendback);
        dialog_theme_imageview_edittextsendback = getChaMView().findViewById(R.id.dialog_theme_imageview_edittextsendback);
        dialog_theme_cardview_edittextsenditem = getChaMView().findViewById(R.id.dialog_theme_cardview_edittextsenditem);
        dialog_theme_imageview_edittextsenditem = getChaMView().findViewById(R.id.dialog_theme_imageview_edittextsenditem);
        dialog_theme_cardview_edittexttoolbarback = getChaMView().findViewById(R.id.dialog_theme_cardview_edittexttoolbarback);
        dialog_theme_imageview_edittexttoolbarback = getChaMView().findViewById(R.id.dialog_theme_imageview_edittexttoolbarback);
        dialog_theme_cardview_edittexttoolbaritem = getChaMView().findViewById(R.id.dialog_theme_cardview_edittexttoolbaritem);
        dialog_theme_imageview_edittexttoolbaritem = getChaMView().findViewById(R.id.dialog_theme_imageview_edittexttoolbaritem);
        dialog_theme_cardview_windownavbarback = getChaMView().findViewById(R.id.dialog_theme_cardview_windownavbarback);
        dialog_theme_imageview_windownavbarback = getChaMView().findViewById(R.id.dialog_theme_imageview_windownavbarback);
        dialog_theme_cardview_windowstatusbarback = getChaMView().findViewById(R.id.dialog_theme_cardview_windowstatusbarback);
        dialog_theme_imageview_windowstatusbarback = getChaMView().findViewById(R.id.dialog_theme_imageview_windowstatusbarback);
        dialog_theme_cardview_windowchatimage = getChaMView().findViewById(R.id.dialog_theme_cardview_windowchatimage);
        dialog_theme_imageview_windowchatimage = getChaMView().findViewById(R.id.dialog_theme_imageview_windowchatimage);
        dialog_theme_cardview_windowchatfont = getChaMView().findViewById(R.id.dialog_theme_cardview_windowchatfont);
        dialog_theme_textview_windowchatfont = getChaMView().findViewById(R.id.dialog_theme_textview_windowchatfont);
    }

    private void init_toolbar(){
        dialog_theme_toolbar.setNavigationIcon(getChaMActivity().getResources().getDrawable(R.drawable.ic_back));
        dialog_theme_toolbar.setNavigationOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                close_dialog();
            }
        });
        dialog_theme_toolbar.inflateMenu(R.menu.menu_checksend);
        dialog_theme_toolbar.setOnMenuItemClickListener(new Toolbar.OnMenuItemClickListener() {
            @Override
            public boolean onMenuItemClick(MenuItem item) {
                switch (item.getItemId()){
                    case R.id.menu_checksend_check:
                        save_theme();
                        ChaMAPI.UserInterface.Do.restart(getChaMActivity(), StartActivity.class);
                        break;
                    case R.id.menu_checksend_send:
                        send_theme();
                        break;
                }
                return false;
            }
        });
    }

    private void init_content(){
        dialog_theme_cardview_mainback.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_mainback.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_mainback.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_mainitem.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_mainitem.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_mainitem.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_toolbarback.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_toolbarback.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_toolbarback.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_toolbaritem.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_toolbaritem.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_toolbaritem.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_buttonmainback.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_buttonmainback.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_buttonmainback.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_buttonmainitem.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_buttonmainitem.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_buttonmainitem.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_buttonsendback.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_buttonsendback.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_buttonsendback.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_buttonsenditem.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_buttonsenditem.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_buttonsenditem.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_cardmainback.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_cardmainback.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_cardmainback.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_cardmainitem.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_cardmainitem.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_cardmainitem.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_cardsendback.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_cardsendback.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_cardsendback.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_cardsenditem.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_cardsenditem.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_cardsenditem.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_cardchatmeback.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_cardchatmeback.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_cardchatmeback.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_cardchatmeitem.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_cardchatmeitem.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_cardchatmeitem.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_cardchatyouback.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_cardchatyouback.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_cardchatyouback.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_cardchatyouitem.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_cardchatyouitem.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_cardchatyouitem.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_edittextmainback.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_edittextmainback.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_edittextmainback.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_edittextmainitem.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_edittextmainitem.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_edittextmainitem.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_edittextsendback.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_edittextsendback.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_edittextsendback.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_edittextsenditem.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_edittextsenditem.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_edittextsenditem.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_edittexttoolbarback.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_edittexttoolbarback.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_edittexttoolbarback.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_edittexttoolbaritem.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_edittexttoolbaritem.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_edittexttoolbaritem.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_windownavbarback.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_windownavbarback.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_windownavbarback.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_windowstatusbarback.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_windowstatusbarback.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_windowstatusbarback.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_windowchatimage.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                // select image
                ChaMAPI.UserInterface.Input.color(getChaMActivity(), ((ColorDrawable) dialog_theme_imageview_windowchatimage.getBackground()).getColor(), new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_imageview_windowchatimage.setBackgroundColor(result);
                    }
                });
            }
        });
        dialog_theme_cardview_windowchatfont.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ChaMAPI.UserInterface.Input.number(getChaMActivity(), Integer.parseInt(dialog_theme_textview_windowchatfont.getText().toString()), 5, 30, new ChaMCoreAPI.ChaMAPIInterface<Integer>() {
                    @Override
                    public void onResult(Integer result) {
                        dialog_theme_textview_windowchatfont.setText(result);
                    }
                });
            }
        });
    }

    private void load_content(){
        if(link == null){
            dialog_theme_imageview_mainback.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_MAIN_BACK))
            );
            dialog_theme_imageview_mainitem.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_MAIN_ITEM))
            );
            dialog_theme_imageview_toolbarback.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_TOOLBAR_BACK))
            );
            dialog_theme_imageview_toolbaritem.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_TOOLBAR_ITEM))
            );
            dialog_theme_imageview_buttonmainback.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_MAIN_BACK))
            );
            dialog_theme_imageview_buttonmainitem.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_MAIN_ITEM))
            );
            dialog_theme_imageview_buttonsendback.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_SEND_BACK))
            );
            dialog_theme_imageview_buttonsenditem.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_SEND_ITEM))
            );
            dialog_theme_imageview_cardmainback.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_MAIN_BACK))
            );
            dialog_theme_imageview_cardmainitem.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_MAIN_ITEM))
            );
            dialog_theme_imageview_cardsendback.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_SEND_BACK))
            );
            dialog_theme_imageview_cardsenditem.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_SEND_ITEM))
            );
            dialog_theme_imageview_cardchatmeback.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATME_BACK))
            );
            dialog_theme_imageview_cardchatmeitem.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATME_ITEM))
            );
            dialog_theme_imageview_cardchatyouback.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATYOU_BACK))
            );
            dialog_theme_imageview_cardchatyouitem.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATYOU_ITEM))
            );
            dialog_theme_imageview_edittextmainback.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_EDITTEXT_MAIN_BACK))
            );
            dialog_theme_imageview_edittextmainitem.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_EDITTEXT_MAIN_ITEM))
            );
            dialog_theme_imageview_edittextsendback.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_EDITTEXT_SEND_BACK))
            );
            dialog_theme_imageview_edittextsenditem.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_EDITTEXT_SEND_ITEM))
            );
            dialog_theme_imageview_edittexttoolbarback.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_EDITTEXT_TOOLBAR_BACK))
            );
            dialog_theme_imageview_edittexttoolbaritem.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_EDITTEXT_TOOLBAR_ITEM))
            );
            dialog_theme_imageview_windownavbarback.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_WINDOW_NAVBAR_BACK))
            );
            dialog_theme_imageview_windowstatusbarback.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_WINDOW_STATUSBAR_BACK))
            );
            dialog_theme_imageview_windowchatimage.setBackgroundColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_WINDOW_CHAT_IMAGE))
            );
            dialog_theme_textview_windowchatfont.setText(
                    ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_WINDOW_CHAT_FONT)
            );
        }else{
            try{
                JSONObject theme = new JSONObject(ChaMAPI.UserInterface.Do.mmap(ChaMCoreAPI.Interface.Stream.file(link,0)));
                dialog_theme_imageview_mainback.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_MAIN_BACK"))
                );
                dialog_theme_imageview_mainitem.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_MAIN_ITEM"))
                );
                dialog_theme_imageview_toolbarback.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_TOOLBAR_BACK"))
                );
                dialog_theme_imageview_toolbaritem.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_TOOLBAR_ITEM"))
                );
                dialog_theme_imageview_buttonmainback.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_BUTTON_MAIN_BACK"))
                );
                dialog_theme_imageview_buttonmainitem.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_BUTTON_MAIN_ITEM"))
                );
                dialog_theme_imageview_buttonsendback.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_BUTTON_SEND_BACK"))
                );
                dialog_theme_imageview_buttonsenditem.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_BUTTON_SEND_ITEM"))
                );
                dialog_theme_imageview_cardmainback.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_CARD_MAIN_BACK"))
                );
                dialog_theme_imageview_cardmainitem.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_CARD_MAIN_ITEM"))
                );
                dialog_theme_imageview_cardsendback.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_CARD_SEND_BACK"))
                );
                dialog_theme_imageview_cardsenditem.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_CARD_SEND_ITEM"))
                );
                dialog_theme_imageview_cardchatmeback.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_CARD_CHATME_BACK"))
                );
                dialog_theme_imageview_cardchatmeitem.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_CARD_CHATME_ITEM"))
                );
                dialog_theme_imageview_cardchatyouback.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_CARD_CHATYOU_BACK"))
                );
                dialog_theme_imageview_cardchatyouitem.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_CARD_CHATYOU_ITEM"))
                );
                dialog_theme_imageview_edittextmainback.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_EDITTEXT_MAIN_BACK"))
                );
                dialog_theme_imageview_edittextmainitem.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_EDITTEXT_MAIN_ITEM"))
                );
                dialog_theme_imageview_edittextsendback.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_EDITTEXT_SEND_BACK"))
                );
                dialog_theme_imageview_edittextsenditem.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_EDITTEXT_SEND_ITEM"))
                );
                dialog_theme_imageview_edittexttoolbarback.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_EDITTEXT_TOOLBAR_BACK"))
                );
                dialog_theme_imageview_edittexttoolbaritem.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_EDITTEXT_TOOLBAR_ITEM"))
                );
                dialog_theme_imageview_windownavbarback.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_WINDOW_NAVBAR_BACK"))
                );
                dialog_theme_imageview_windowstatusbarback.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_WINDOW_STATUSBAR_BACK"))
                );
                dialog_theme_imageview_windowchatimage.setBackgroundColor(
                        Integer.parseInt(theme.getString("THEME_WINDOW_CHAT_IMAGE"))
                );
                dialog_theme_textview_windowchatfont.setText(
                        theme.getString("THEME_WINDOW_CHAT_FONT")
                );
            }catch (Exception ignored){}
        }
    }

    private void save_theme(){
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_MAIN_BACK,((ColorDrawable)dialog_theme_imageview_mainback.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_MAIN_ITEM,((ColorDrawable)dialog_theme_imageview_mainitem.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_TOOLBAR_BACK,((ColorDrawable)dialog_theme_imageview_toolbarback.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_TOOLBAR_ITEM,((ColorDrawable)dialog_theme_imageview_toolbaritem.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_MAIN_BACK,((ColorDrawable)dialog_theme_imageview_buttonmainback.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_MAIN_ITEM,((ColorDrawable)dialog_theme_imageview_buttonmainitem.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_SEND_BACK,((ColorDrawable)dialog_theme_imageview_buttonsendback.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_BUTTON_SEND_ITEM,((ColorDrawable)dialog_theme_imageview_buttonsenditem.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_MAIN_BACK,((ColorDrawable)dialog_theme_imageview_cardmainback.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_MAIN_ITEM,((ColorDrawable)dialog_theme_imageview_cardmainitem.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_SEND_BACK,((ColorDrawable)dialog_theme_imageview_cardsendback.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_SEND_ITEM,((ColorDrawable)dialog_theme_imageview_cardsenditem.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATME_BACK,((ColorDrawable)dialog_theme_imageview_cardchatmeback.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATME_ITEM,((ColorDrawable)dialog_theme_imageview_cardchatmeitem.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATYOU_BACK,((ColorDrawable)dialog_theme_imageview_cardchatyouback.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_CARD_CHATYOU_ITEM,((ColorDrawable)dialog_theme_imageview_cardchatyouitem.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_EDITTEXT_MAIN_BACK,((ColorDrawable)dialog_theme_imageview_edittextmainback.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_EDITTEXT_MAIN_ITEM,((ColorDrawable)dialog_theme_imageview_edittextmainitem.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_EDITTEXT_SEND_BACK,((ColorDrawable)dialog_theme_imageview_edittextsendback.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_EDITTEXT_SEND_ITEM,((ColorDrawable)dialog_theme_imageview_edittextsenditem.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_EDITTEXT_TOOLBAR_BACK,((ColorDrawable)dialog_theme_imageview_edittexttoolbarback.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_EDITTEXT_TOOLBAR_ITEM,((ColorDrawable)dialog_theme_imageview_edittexttoolbaritem.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_WINDOW_NAVBAR_BACK,((ColorDrawable)dialog_theme_imageview_windownavbarback.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_WINDOW_STATUSBAR_BACK,((ColorDrawable)dialog_theme_imageview_windowstatusbarback.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_WINDOW_CHAT_IMAGE,((ColorDrawable)dialog_theme_imageview_windowchatimage.getBackground()).getColor()+"");
        ChaMCoreAPI.Interface.self_set(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_WINDOW_CHAT_FONT,dialog_theme_textview_windowchatfont.getText().toString());
    }

    private void send_theme(){
        try{
            final JSONObject result_theme = new JSONObject();
            result_theme.put("THEME_MAIN_BACK",((ColorDrawable) dialog_theme_imageview_mainback.getBackground()).getColor());
            result_theme.put("THEME_MAIN_ITEM",((ColorDrawable) dialog_theme_imageview_mainitem.getBackground()).getColor());
            result_theme.put("THEME_TOOLBAR_BACK",((ColorDrawable) dialog_theme_imageview_toolbarback.getBackground()).getColor());
            result_theme.put("THEME_TOOLBAR_ITEM",((ColorDrawable) dialog_theme_imageview_toolbaritem.getBackground()).getColor());
            result_theme.put("THEME_BUTTON_MAIN_BACK",((ColorDrawable) dialog_theme_imageview_buttonmainback.getBackground()).getColor());
            result_theme.put("THEME_BUTTON_MAIN_ITEM",((ColorDrawable) dialog_theme_imageview_buttonmainitem.getBackground()).getColor());
            result_theme.put("THEME_BUTTON_SEND_BACK",((ColorDrawable) dialog_theme_imageview_buttonsendback.getBackground()).getColor());
            result_theme.put("THEME_BUTTON_SEND_ITEM",((ColorDrawable) dialog_theme_imageview_buttonsenditem.getBackground()).getColor());
            result_theme.put("THEME_CARD_MAIN_BACK",((ColorDrawable) dialog_theme_imageview_cardmainback.getBackground()).getColor());
            result_theme.put("THEME_CARD_MAIN_ITEM",((ColorDrawable) dialog_theme_imageview_cardmainitem.getBackground()).getColor());
            result_theme.put("THEME_CARD_SEND_BACK",((ColorDrawable) dialog_theme_imageview_cardsendback.getBackground()).getColor());
            result_theme.put("THEME_CARD_SEND_ITEM",((ColorDrawable) dialog_theme_imageview_cardsenditem.getBackground()).getColor());
            result_theme.put("THEME_CARD_CHATME_BACK",((ColorDrawable) dialog_theme_imageview_cardchatmeback.getBackground()).getColor());
            result_theme.put("THEME_CARD_CHATME_ITEM",((ColorDrawable) dialog_theme_imageview_cardchatmeitem.getBackground()).getColor());
            result_theme.put("THEME_CARD_CHATYOU_BACK",((ColorDrawable) dialog_theme_imageview_cardchatyouback.getBackground()).getColor());
            result_theme.put("THEME_CARD_CHATYOU_ITEM",((ColorDrawable) dialog_theme_imageview_cardchatyouitem.getBackground()).getColor());
            result_theme.put("THEME_EDITTEXT_MAIN_BACK",((ColorDrawable) dialog_theme_imageview_edittextmainback.getBackground()).getColor());
            result_theme.put("THEME_EDITTEXT_MAIN_ITEM",((ColorDrawable) dialog_theme_imageview_edittextmainitem.getBackground()).getColor());
            result_theme.put("THEME_EDITTEXT_SEND_BACK",((ColorDrawable) dialog_theme_imageview_edittextsendback.getBackground()).getColor());
            result_theme.put("THEME_EDITTEXT_SEND_ITEM",((ColorDrawable) dialog_theme_imageview_edittextsenditem.getBackground()).getColor());
            result_theme.put("THEME_EDITTEXT_TOOLBAR_BACK",((ColorDrawable) dialog_theme_imageview_edittexttoolbarback.getBackground()).getColor());
            result_theme.put("THEME_EDITTEXT_TOOLBAR_ITEM",((ColorDrawable) dialog_theme_imageview_edittexttoolbaritem.getBackground()).getColor());
            result_theme.put("THEME_WINDOW_NAVBAR_BACK",((ColorDrawable) dialog_theme_imageview_windownavbarback.getBackground()).getColor());
            result_theme.put("THEME_WINDOW_STATUSBAR_BACK",((ColorDrawable) dialog_theme_imageview_windowstatusbarback.getBackground()).getColor());
            result_theme.put("THEME_WINDOW_CHAT_IMAGE",((ColorDrawable) dialog_theme_imageview_windowchatimage.getBackground()).getColor());
            result_theme.put("THEME_WINDOW_CHAT_FONT",dialog_theme_textview_windowchatfont.getText());
            ChaMAPI.UserInterface.Input.connection(getChaMActivity(), new ChaMCoreAPI.ChaMAPIInterface<String>() {
                @Override
                public void onResult(String result) {
                    ChaMCoreAPI.Interface.Protocol.message_set(null,result,"THEME","","",result_theme.toString(),"",null);
                }
            });
        }catch (Exception ignored){}
    }
}
