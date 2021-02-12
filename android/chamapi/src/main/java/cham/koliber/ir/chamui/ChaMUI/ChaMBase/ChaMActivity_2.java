package cham.koliber.ir.chamui.ChaMUI.ChaMBase;

import android.content.res.Configuration;
import android.os.Build;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.v4.app.Fragment;
import android.support.v4.view.ViewCompat;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.app.AppCompatDelegate;
import android.view.MotionEvent;
import java.util.Locale;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.R;

public abstract class ChaMActivity extends AppCompatActivity{

    private ChaMCoreAPI.ChaMAPIInterface<Boolean> api_interface;

    static {
        AppCompatDelegate.setCompatVectorFromResourcesEnabled(true);
    }

    @Override
    protected void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        // check storage permissions
        if(!ChaMCoreAPI.Permission.check(this, ChaMCoreAPI.Permission.PermissionType.PERMISSION_TYPE_STORAGE)){
            ChaMCoreAPI.Permission.grant(this, ChaMCoreAPI.Permission.PermissionType.PERMISSION_TYPE_STORAGE, true, new ChaMCoreAPI.ChaMAPIInterface<Boolean>() {
                @Override
                public void onResult(Boolean result) {
                    if(ChaMCoreAPI.Permission.check(ChaMActivity.this, ChaMCoreAPI.Permission.PermissionType.PERMISSION_TYPE_STORAGE)){
                        ChaMCoreAPI.run(getApplicationContext());
                        create_activity(savedInstanceState);
                    }else{
                        finish();
                    }
                }
            });
        }else{
            create_activity(savedInstanceState);
        }
    }

    private void create_activity(Bundle savedInstanceState){
        init_language();
        init_window();
        init_activity();
        init_direction();
        if(savedInstanceState == null){
            start_activity();
        }
    }

    private void init_language(){
        Locale locale = new Locale(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.UI_LANGUAGE));
        Locale.setDefault(locale);
        Configuration config = new Configuration();
        config.locale = locale;
        getBaseContext().getResources().updateConfiguration(config, getBaseContext().getResources().getDisplayMetrics());
    }

    private void init_window(){
        setTheme(R.style.ChaM_Base_ChaMActivity);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
            getWindow().setStatusBarColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_WINDOW_STATUSBAR_BACK))
            );
            getWindow().setNavigationBarColor(
                    Integer.parseInt(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.THEME_WINDOW_NAVBAR_BACK))
            );
        }
    }

    private void init_direction(){
        Locale locale = new Locale(ChaMCoreAPI.Interface.self_get(ChaMCoreAPI.Interface.ChaMInterfaceKey.UI_LANGUAGE));
        final int directionality = Character.getDirectionality(locale.getDisplayName().charAt(0));
        ViewCompat.setLayoutDirection(getWindow().getDecorView().getRootView() , directionality == Character.DIRECTIONALITY_RIGHT_TO_LEFT || directionality == Character.DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC ? ViewCompat.LAYOUT_DIRECTION_RTL : ViewCompat.LAYOUT_DIRECTION_LTR);
    }

    public abstract void init_activity();

    public abstract void start_activity();

    public ChaMActivity getChaMActivity(){
        return this;
    }

    @Override
    public void onResume() {
        Runtime.getRuntime().gc();
        System.gc();
        super.onResume();
    }

    @Override
    public boolean dispatchTouchEvent(MotionEvent ev) {
        if(getSupportFragmentManager().getFragments() != null){
            Fragment fragment;
            for(int cursor = getSupportFragmentManager().getFragments().size()-1 ; cursor >= 0 ; cursor--){
                fragment = getSupportFragmentManager().getFragments().get(cursor);
                if(fragment instanceof ChaMFragment){
                    ((ChaMFragment) fragment).onTouch(ev);
                    break;
                }
            }
        }
        return super.dispatchTouchEvent(ev);
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        if(api_interface != null){
            api_interface.onResult(true);
        }
    }

    public void setPermissionListener(ChaMCoreAPI.ChaMAPIInterface<Boolean> api_interface){
        this.api_interface = api_interface;
    }
}
