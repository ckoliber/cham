package cham.koliber.ir.chamui.ChaMUI.ChaMCore;

import android.Manifest;
import android.annotation.SuppressLint;
import android.content.Context;
import android.content.pm.PackageManager;
import android.os.Build;
import android.os.Environment;
import android.support.v4.app.ActivityCompat;
import android.support.v4.content.ContextCompat;
import android.util.DisplayMetrics;
import android.util.Pair;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMActivity;

public class ChaMCoreAPI {

    @SuppressLint("StaticFieldLeak")
    private static Context context;

    public static void run(Context context){
        ChaMCoreAPI.context = context;
        ChaMCoreAPI.Interface.run();
        ChaMCoreAPI.Executor.run();
    }

    public static class Permission{

        public enum PermissionType{
            PERMISSION_TYPE_NETWORK,
            PERMISSION_TYPE_STORAGE,
            PERMISSION_TYPE_CAMERA,
            PERMISSION_TYPE_MIC,
            PERMISSION_TYPE_LOCATION,
            PERMISSION_TYPE_CONTACT,
            PERMISSION_TYPE_VIBRATE
        }

        private static String[] permissions(PermissionType type){
            switch (type){
                case PERMISSION_TYPE_NETWORK:
                    return new String[]{
                            Manifest.permission.ACCESS_NETWORK_STATE,
                            Manifest.permission.CHANGE_NETWORK_STATE,
                            Manifest.permission.INTERNET
                    };
                case PERMISSION_TYPE_STORAGE:
                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN) {
                        return new String[]{
                                Manifest.permission.WRITE_EXTERNAL_STORAGE,
                                Manifest.permission.READ_EXTERNAL_STORAGE
                        };
                    }else{
                        return new String[]{
                                Manifest.permission.WRITE_EXTERNAL_STORAGE
                        };
                    }
                case PERMISSION_TYPE_CAMERA:
                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
                        return new String[]{
                                Manifest.permission.CAMERA,
                                Manifest.permission.CAPTURE_VIDEO_OUTPUT
                        };
                    }else{
                        return new String[]{
                                Manifest.permission.CAMERA
                        };
                    }
                case PERMISSION_TYPE_MIC:
                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
                        return new String[]{
                                Manifest.permission.RECORD_AUDIO,
                                Manifest.permission.CAPTURE_AUDIO_OUTPUT,
                                Manifest.permission.MODIFY_AUDIO_SETTINGS
                        };
                    }else{
                        return new String[]{
                                Manifest.permission.RECORD_AUDIO,
                                Manifest.permission.MODIFY_AUDIO_SETTINGS
                        };
                    }
                case PERMISSION_TYPE_LOCATION:
                    return new String[]{
                            Manifest.permission.ACCESS_FINE_LOCATION,
                            Manifest.permission.ACCESS_COARSE_LOCATION
                    };
                case PERMISSION_TYPE_CONTACT:
                    return new String[]{
                            Manifest.permission.READ_CONTACTS,
                            Manifest.permission.WRITE_CONTACTS
                    };
                default:
                    return new String[]{
                            Manifest.permission.VIBRATE
                    };
            }
        }

        private static void request(final ChaMActivity activity , final PermissionType type , final ChaMAPIInterface<Boolean> api_interface){
            if(!check(activity,type)){
                activity.setPermissionListener(new ChaMAPIInterface<Boolean>() {
                    @Override
                    public void onResult(Boolean result) {
                        api_interface.onResult(check(activity,type));
                    }
                });
                ActivityCompat.requestPermissions(activity,permissions(type),1418);
            }else{
                api_interface.onResult(true);
            }
        }

        public static boolean check(ChaMActivity activity , PermissionType type){
            for(String permission : permissions(type)){
                if(ContextCompat.checkSelfPermission(activity,permission) != PackageManager.PERMISSION_GRANTED){
                    return false;
                }
            }
            return true;
        }

        public static void grant(final ChaMActivity activity , final PermissionType type, final boolean force, final ChaMAPIInterface<Boolean> api_interface){
            if(force){
                request(activity, type, new ChaMAPIInterface<Boolean>() {
                    @Override
                    public void onResult(Boolean result) {
                        if(result){
                            api_interface.onResult(true);
                        }else{
                            grant(activity,type,force,api_interface);
                        }
                    }
                });
            }else{
                request(activity,type,api_interface);
            }
        }

    }

    public static class Interface{

        public enum ChaMInterfaceKey{
            SETTING_ID,
            SETTING_SCODE,
            SETTING_PHONE,

            UI_LANGUAGE,
            UI_PIN,
            UI_LOCK,

            STREAM_DOWNLOAD,
            STREAM_UPLOAD,

            // MAIN LAYOUT
            THEME_MAIN_BACK,
            THEME_MAIN_ITEM,

            // TOOLBAR
            THEME_TOOLBAR_BACK,
            THEME_TOOLBAR_ITEM,

            // BUTTON ON MAIN LAYOUT
            THEME_BUTTON_MAIN_BACK,
            THEME_BUTTON_MAIN_ITEM,
            // BUTTON ON SEND CARD
            THEME_BUTTON_SEND_BACK,
            THEME_BUTTON_SEND_ITEM,

            // CARD ON MAIN LAYOUT
            THEME_CARD_MAIN_BACK,
            THEME_CARD_MAIN_ITEM,
            // CARD ON SEND LAYOUT
            THEME_CARD_SEND_BACK,
            THEME_CARD_SEND_ITEM,
            // CHAT ME CARD
            THEME_CARD_CHATME_BACK,
            THEME_CARD_CHATME_ITEM,
            // CHAT YOUT CARD
            THEME_CARD_CHATYOU_BACK,
            THEME_CARD_CHATYOU_ITEM,

            // EDITTEXT ON MAIN LAYOUT
            THEME_EDITTEXT_MAIN_BACK,
            THEME_EDITTEXT_MAIN_ITEM,
            // EDITTEXT ON SEND CARD
            THEME_EDITTEXT_SEND_BACK,
            THEME_EDITTEXT_SEND_ITEM,
            // EDITTEXT ON TOOLBAR
            THEME_EDITTEXT_TOOLBAR_BACK,
            THEME_EDITTEXT_TOOLBAR_ITEM,

            // WINDOW GLOBAL
            THEME_WINDOW_NAVBAR_BACK,
            THEME_WINDOW_STATUSBAR_BACK,
            THEME_WINDOW_CHAT_IMAGE,
            THEME_WINDOW_CHAT_FONT
        }

        private static ChaMNativeInterface cham_native_interface;

        public static void run(){
            if(cham_native_interface != null){
                cham_native_interface.interface_stop();
                cham_native_interface.destruct();
            }
            cham_native_interface = new ChaMNativeInterface();
            cham_native_interface.construct();
            cham_native_interface.interface_start();
        }

        public static int state(){
            return cham_native_interface.interface_state();
        }
        public static int bundle(ChaMNativeInterface.ChaMInterfaceInterface interface_interface){
            return cham_native_interface.interface_bundle(0,interface_interface);
        }

        public static int self_set(ChaMInterfaceKey key, String value){
            return cham_native_interface.interface_self_set(key.toString(),value);
        }
        public static String self_get(ChaMInterfaceKey key){
            return cham_native_interface.interface_self_get(key.toString());
        }
        public static int self_set(String key, String value) {
            return cham_native_interface.interface_self_set(key,value);
        }
        public static String self_get(String key){
            return cham_native_interface.interface_self_get(key);
        }

        public static class Protocol{

            public static int bundle(final ProtocolInterface protocol_interface){
                return cham_native_interface.protocol_bundle(2, new ChaMNativeInterface.ChaMProtocolInterface() {
                    @Override
                    public void onMessage(String token, String request, String response) {
                        try{
                            if(response.length() > 0){
                                JSONObject response_object = new JSONObject(response);
                                if(!response_object.has("ERROR")){
                                    if(protocol_interface != null){
                                        protocol_interface.onResponse(response_object);
                                    }
                                }else{
                                    if(protocol_interface != null){
                                        protocol_interface.onError(response_object.getString("ERROR"));
                                    }
                                }
                            }else{
                                if(protocol_interface != null){
                                    protocol_interface.onRequest(token,new JSONObject(request));
                                }
                            }
                        }catch (Exception ignored){
                            if(protocol_interface != null){
                                protocol_interface.onError(ignored.getMessage());
                            }
                        }
                    }
                });
            }
            public static int update(String token, final ProtocolInterface protocol_interface){
                return cham_native_interface.protocol_update(token,new ChaMNativeInterface.ChaMProtocolInterface() {
                    @Override
                    public void onMessage(String token, String request, String response) {
                        try{
                            if(response.length() > 0){
                                JSONObject response_object = new JSONObject(response);
                                if(!response_object.has("ERROR")){
                                    if(protocol_interface != null){
                                        protocol_interface.onResponse(response_object);
                                    }
                                }else{
                                    if(protocol_interface != null){
                                        protocol_interface.onError(response_object.getString("ERROR"));
                                    }
                                }
                            }else{
                                if(protocol_interface != null){
                                    protocol_interface.onRequest(token,new JSONObject(request));
                                }
                            }
                        }catch (Exception ignored){
                            if(protocol_interface != null){
                                protocol_interface.onError(ignored.getMessage());
                            }
                        }
                    }
                });
            }

            public static void peer_captcha(String token, String phone, String type, final ProtocolInterface protocol_interface){
                cham_native_interface.protocol_peer_captcha(new ChaMNativeInterface.ChaMProtocolInterface() {
                    @Override
                    public void onMessage(String token, String request, String response) {
                        try{
                            if(response.length() > 0){
                                JSONObject response_object = new JSONObject(response);
                                if(!response_object.has("ERROR")){
                                    if(protocol_interface != null){
                                        protocol_interface.onResponse(response_object);
                                    }
                                }else{
                                    if(protocol_interface != null){
                                        protocol_interface.onError(response_object.getString("ERROR"));
                                    }
                                }
                            }else{
                                if(protocol_interface != null){
                                    protocol_interface.onRequest(token,new JSONObject(request));
                                }
                            }
                        }catch (Exception ignored){
                            if(protocol_interface != null){
                                protocol_interface.onError(ignored.getMessage());
                            }
                        }
                    }
                },token,phone,type);
            }
            public static void peer_login(String token, String id, String scode, String platform, String version, final ProtocolInterface protocol_interface){
                cham_native_interface.protocol_peer_login(new ChaMNativeInterface.ChaMProtocolInterface() {
                    @Override
                    public void onMessage(String token, String request, String response) {
                        try{
                            if(response.length() > 0){
                                JSONObject response_object = new JSONObject(response);
                                if(!response_object.has("ERROR")){
                                    if(protocol_interface != null){
                                        protocol_interface.onResponse(response_object);
                                    }
                                }else{
                                    if(protocol_interface != null){
                                        protocol_interface.onError(response_object.getString("ERROR"));
                                    }
                                }
                            }else{
                                if(protocol_interface != null){
                                    protocol_interface.onRequest(token,new JSONObject(request));
                                }
                            }
                        }catch (Exception ignored){
                            if(protocol_interface != null){
                                protocol_interface.onError(ignored.getMessage());
                            }
                        }
                    }
                },token,id,scode,platform,version);
            }
            public static void peer_logout(String token, final ProtocolInterface protocol_interface){
                cham_native_interface.protocol_peer_logout(new ChaMNativeInterface.ChaMProtocolInterface() {
                    @Override
                    public void onMessage(String token, String request, String response) {
                        try{
                            if(response.length() > 0){
                                JSONObject response_object = new JSONObject(response);
                                if(!response_object.has("ERROR")){
                                    if(protocol_interface != null){
                                        protocol_interface.onResponse(response_object);
                                    }
                                }else{
                                    if(protocol_interface != null){
                                        protocol_interface.onError(response_object.getString("ERROR"));
                                    }
                                }
                            }else{
                                if(protocol_interface != null){
                                    protocol_interface.onRequest(token,new JSONObject(request));
                                }
                            }
                        }catch (Exception ignored){
                            if(protocol_interface != null){
                                protocol_interface.onError(ignored.getMessage());
                            }
                        }
                    }
                },token);
            }
            public static void peer_create(String token, String phone, String captcha, final ProtocolInterface protocol_interface){
                cham_native_interface.protocol_peer_create(new ChaMNativeInterface.ChaMProtocolInterface() {
                    @Override
                    public void onMessage(String token, String request, String response) {
                        try{
                            if(response.length() > 0){
                                JSONObject response_object = new JSONObject(response);
                                if(!response_object.has("ERROR")){
                                    if(protocol_interface != null){
                                        protocol_interface.onResponse(response_object);
                                    }
                                }else{
                                    if(protocol_interface != null){
                                        protocol_interface.onError(response_object.getString("ERROR"));
                                    }
                                }
                            }else{
                                if(protocol_interface != null){
                                    protocol_interface.onRequest(token,new JSONObject(request));
                                }
                            }
                        }catch (Exception ignored){
                            if(protocol_interface != null){
                                protocol_interface.onError(ignored.getMessage());
                            }
                        }
                    }
                },token,phone,captcha);
            }
            public static void peer_delete(String token, final ProtocolInterface protocol_interface){
                cham_native_interface.protocol_peer_delete(new ChaMNativeInterface.ChaMProtocolInterface() {
                    @Override
                    public void onMessage(String token, String request, String response) {
                        try{
                            if(response.length() > 0){
                                JSONObject response_object = new JSONObject(response);
                                if(!response_object.has("ERROR")){
                                    if(protocol_interface != null){
                                        protocol_interface.onResponse(response_object);
                                    }
                                }else{
                                    if(protocol_interface != null){
                                        protocol_interface.onError(response_object.getString("ERROR"));
                                    }
                                }
                            }else{
                                if(protocol_interface != null){
                                    protocol_interface.onRequest(token,new JSONObject(request));
                                }
                            }
                        }catch (Exception ignored){
                            if(protocol_interface != null){
                                protocol_interface.onError(ignored.getMessage());
                            }
                        }
                    }
                },token);
            }
            public static void peer_load(String token, String id, String type, String mode, String param_1, String param_2, int local, int trying, final ProtocolInterface protocol_interface){
                cham_native_interface.protocol_peer_load(new ChaMNativeInterface.ChaMProtocolInterface() {
                    @Override
                    public void onMessage(String token, String request, String response) {
                        try{
                            if(response.length() > 0){
                                JSONObject response_object = new JSONObject(response);
                                if(!response_object.has("ERROR")){
                                    if(protocol_interface != null){
                                        protocol_interface.onResponse(response_object);
                                    }
                                }else{
                                    if(protocol_interface != null){
                                        protocol_interface.onError(response_object.getString("ERROR"));
                                    }
                                }
                            }else{
                                if(protocol_interface != null){
                                    protocol_interface.onRequest(token,new JSONObject(request));
                                }
                            }
                        }catch (Exception ignored){
                            if(protocol_interface != null){
                                protocol_interface.onError(ignored.getMessage());
                            }
                        }
                    }
                },token,id,type,mode,param_1,param_2,local,trying);
            }
            public static void peer_set(String token, String id, String key, String option, String value, String operator, final ProtocolInterface protocol_interface){
                cham_native_interface.protocol_peer_set(new ChaMNativeInterface.ChaMProtocolInterface() {
                    @Override
                    public void onMessage(String token, String request, String response) {
                        try{
                            if(response.length() > 0){
                                JSONObject response_object = new JSONObject(response);
                                if(!response_object.has("ERROR")){
                                    if(protocol_interface != null){
                                        protocol_interface.onResponse(response_object);
                                    }
                                }else{
                                    if(protocol_interface != null){
                                        protocol_interface.onError(response_object.getString("ERROR"));
                                    }
                                }
                            }else{
                                if(protocol_interface != null){
                                    protocol_interface.onRequest(token,new JSONObject(request));
                                }
                            }
                        }catch (Exception ignored){
                            if(protocol_interface != null){
                                protocol_interface.onError(ignored.getMessage());
                            }
                        }
                    }
                },token,id,key,option,value,operator);
            }
            public static void peer_id(String token, String key, String type, final ProtocolInterface protocol_interface){
                cham_native_interface.protocol_peer_id(new ChaMNativeInterface.ChaMProtocolInterface() {
                    @Override
                    public void onMessage(String token, String request, String response) {
                        try{
                            if(response.length() > 0){
                                JSONObject response_object = new JSONObject(response);
                                if(!response_object.has("ERROR")){
                                    if(protocol_interface != null){
                                        protocol_interface.onResponse(response_object);
                                    }
                                }else{
                                    if(protocol_interface != null){
                                        protocol_interface.onError(response_object.getString("ERROR"));
                                    }
                                }
                            }else{
                                if(protocol_interface != null){
                                    protocol_interface.onRequest(token,new JSONObject(request));
                                }
                            }
                        }catch (Exception ignored){
                            if(protocol_interface != null){
                                protocol_interface.onError(ignored.getMessage());
                            }
                        }
                    }
                },token,key,type);
            }

            public static void connection_create(String token, final ProtocolInterface protocol_interface){
                cham_native_interface.protocol_connection_create(new ChaMNativeInterface.ChaMProtocolInterface() {
                    @Override
                    public void onMessage(String token, String request, String response) {
                        try{
                            if(response.length() > 0){
                                JSONObject response_object = new JSONObject(response);
                                if(!response_object.has("ERROR")){
                                    if(protocol_interface != null){
                                        protocol_interface.onResponse(response_object);
                                    }
                                }else{
                                    if(protocol_interface != null){
                                        protocol_interface.onError(response_object.getString("ERROR"));
                                    }
                                }
                            }else{
                                if(protocol_interface != null){
                                    protocol_interface.onRequest(token,new JSONObject(request));
                                }
                            }
                        }catch (Exception ignored){
                            if(protocol_interface != null){
                                protocol_interface.onError(ignored.getMessage());
                            }
                        }
                    }
                },token);
            }
            public static void connection_delete(String token, String id, final ProtocolInterface protocol_interface){
                cham_native_interface.protocol_connection_delete(new ChaMNativeInterface.ChaMProtocolInterface() {
                    @Override
                    public void onMessage(String token, String request, String response) {
                        try{
                            if(response.length() > 0){
                                JSONObject response_object = new JSONObject(response);
                                if(!response_object.has("ERROR")){
                                    if(protocol_interface != null){
                                        protocol_interface.onResponse(response_object);
                                    }
                                }else{
                                    if(protocol_interface != null){
                                        protocol_interface.onError(response_object.getString("ERROR"));
                                    }
                                }
                            }else{
                                if(protocol_interface != null){
                                    protocol_interface.onRequest(token,new JSONObject(request));
                                }
                            }
                        }catch (Exception ignored){
                            if(protocol_interface != null){
                                protocol_interface.onError(ignored.getMessage());
                            }
                        }
                    }
                },token,id);
            }
            public static void connection_set(String token, String id, String key, String option, String value, String operator, final ProtocolInterface protocol_interface){
                cham_native_interface.protocol_connection_set(new ChaMNativeInterface.ChaMProtocolInterface() {
                    @Override
                    public void onMessage(String token, String request, String response) {
                        try{
                            if(response.length() > 0){
                                JSONObject response_object = new JSONObject(response);
                                if(!response_object.has("ERROR")){
                                    if(protocol_interface != null){
                                        protocol_interface.onResponse(response_object);
                                    }
                                }else{
                                    if(protocol_interface != null){
                                        protocol_interface.onError(response_object.getString("ERROR"));
                                    }
                                }
                            }else{
                                if(protocol_interface != null){
                                    protocol_interface.onRequest(token,new JSONObject(request));
                                }
                            }
                        }catch (Exception ignored){
                            if(protocol_interface != null){
                                protocol_interface.onError(ignored.getMessage());
                            }
                        }
                    }
                },token,id,key,option,value,operator);
            }
            public static void connection_id(String token, String key, String type, final ProtocolInterface protocol_interface){
                cham_native_interface.protocol_connection_id(new ChaMNativeInterface.ChaMProtocolInterface() {
                    @Override
                    public void onMessage(String token, String request, String response) {
                        try{
                            if(response.length() > 0){
                                JSONObject response_object = new JSONObject(response);
                                if(!response_object.has("ERROR")){
                                    if(protocol_interface != null){
                                        protocol_interface.onResponse(response_object);
                                    }
                                }else{
                                    if(protocol_interface != null){
                                        protocol_interface.onError(response_object.getString("ERROR"));
                                    }
                                }
                            }else{
                                if(protocol_interface != null){
                                    protocol_interface.onRequest(token,new JSONObject(request));
                                }
                            }
                        }catch (Exception ignored){
                            if(protocol_interface != null){
                                protocol_interface.onError(ignored.getMessage());
                            }
                        }
                    }
                },token,key,type);
            }

            public static void message_set(String token, String conection_id, String message_type, String forward_id, String reply_date, String data_1, String data_2, final ProtocolInterface protocol_interface){
                cham_native_interface.protocol_message_set(new ChaMNativeInterface.ChaMProtocolInterface() {
                    @Override
                    public void onMessage(String token, String request, String response) {
                        try{
                            if(response.length() > 0){
                                JSONObject response_object = new JSONObject(response);
                                if(!response_object.has("ERROR")){
                                    if(protocol_interface != null){
                                        protocol_interface.onResponse(response_object);
                                    }
                                }else{
                                    if(protocol_interface != null){
                                        protocol_interface.onError(response_object.getString("ERROR"));
                                    }
                                }
                            }else{
                                if(protocol_interface != null){
                                    protocol_interface.onRequest(token,new JSONObject(request));
                                }
                            }
                        }catch (Exception ignored){
                            if(protocol_interface != null){
                                protocol_interface.onError(ignored.getMessage());
                            }
                        }
                    }
                },token,conection_id,message_type,forward_id,reply_date,data_1,data_2);
            }
            public static void message_handle(String token, String conection_id, String handle, final ProtocolInterface protocol_interface){
                cham_native_interface.protocol_message_handle(new ChaMNativeInterface.ChaMProtocolInterface() {
                    @Override
                    public void onMessage(String token, String request, String response) {
                        try{
                            if(response.length() > 0){
                                JSONObject response_object = new JSONObject(response);
                                if(!response_object.has("ERROR")){
                                    if(protocol_interface != null){
                                        protocol_interface.onResponse(response_object);
                                    }
                                }else{
                                    if(protocol_interface != null){
                                        protocol_interface.onError(response_object.getString("ERROR"));
                                    }
                                }
                            }else{
                                if(protocol_interface != null){
                                    protocol_interface.onRequest(token,new JSONObject(request));
                                }
                            }
                        }catch (Exception ignored){
                            if(protocol_interface != null){
                                protocol_interface.onError(ignored.getMessage());
                            }
                        }
                    }
                },token,conection_id,handle);
            }

            public interface ProtocolInterface{
                void onRequest(String token, JSONObject request);
                void onError(String error);
                void onResponse(JSONObject response);
            }

        }

        public static class Stream{

            public static int update(String token, final StreamInterface stream_interface){
                return cham_native_interface.stream_update(token, new ChaMNativeInterface.ChaMStreamInterface() {
                    @Override
                    public void onStart(String link) {
                        if(stream_interface != null){
                            stream_interface.onStart(link);
                        }
                    }

                    @Override
                    public void onOpen(String link) {
                        if(stream_interface != null){
                            stream_interface.onOpen(link);
                        }
                    }

                    @Override
                    public void onHandshake(String link) {
                        if(stream_interface != null){
                            stream_interface.onHandshake(link);
                        }
                    }

                    @Override
                    public void onData(String link, long seek) {
                        if(stream_interface != null){
                            stream_interface.onData(link,seek);
                        }
                    }

                    @Override
                    public void onStop(String link) {
                        if(stream_interface != null){
                            stream_interface.onStop(link);
                        }
                    }
                });
            }
            public static void set(String link, String path, int calltime, final StreamInterface stream_interface){
                cham_native_interface.stream_set(new ChaMNativeInterface.ChaMStreamInterface() {
                    @Override
                    public void onStart(String link) {
                        if(stream_interface != null){
                            stream_interface.onStart(link);
                        }
                    }

                    @Override
                    public void onOpen(String link) {
                        if(stream_interface != null){
                            stream_interface.onOpen(link);
                        }
                    }

                    @Override
                    public void onHandshake(String link) {
                        if(stream_interface != null){
                            stream_interface.onHandshake(link);
                        }
                    }

                    @Override
                    public void onData(String link, long seek) {
                        if(stream_interface != null){
                            stream_interface.onData(link,seek);
                        }
                    }

                    @Override
                    public void onStop(String link) {
                        if(stream_interface != null){
                            stream_interface.onStop(link);
                        }
                    }
                },link,path,calltime);
            }
            public static void get(String link, String path, int calltime, final StreamInterface stream_interface){
                cham_native_interface.stream_get(new ChaMNativeInterface.ChaMStreamInterface() {
                    @Override
                    public void onStart(String link) {
                        if(stream_interface != null){
                            stream_interface.onStart(link);
                        }
                    }

                    @Override
                    public void onOpen(String link) {
                        if(stream_interface != null){
                            stream_interface.onOpen(link);
                        }
                    }

                    @Override
                    public void onHandshake(String link) {
                        if(stream_interface != null){
                            stream_interface.onHandshake(link);
                        }
                    }

                    @Override
                    public void onData(String link, long seek) {
                        if(stream_interface != null){
                            stream_interface.onData(link,seek);
                        }
                    }

                    @Override
                    public void onStop(String link) {
                        if(stream_interface != null){
                            stream_interface.onStop(link);
                        }
                    }
                },link,path,calltime);
            }
            public static JSONObject sets(){
                try{
                    return new JSONObject(cham_native_interface.stream_sets());
                }catch (Exception ignored){
                    return null;
                }
            }
            public static JSONObject gets(){
                try{
                    return new JSONObject(cham_native_interface.stream_gets());
                }catch (Exception ignored){
                    return null;
                }
            }
            public static int cancel(String link){
                return cham_native_interface.stream_cancel(link);
            }
            public static JSONObject state(String link){
                try{
                    return new JSONObject(cham_native_interface.stream_state(link));
                }catch (Exception ignored){
                    return null;
                }
            }
            public static int clear(){
                return cham_native_interface.stream_clear();
            }
            public static long size(){
                return cham_native_interface.stream_size();
            }
            public static String getdata(String link){
                return cham_native_interface.stream_getdata(link);
            }
            public static int setdata(String link, String state){
                return cham_native_interface.stream_setdata(link,state);
            }
            public static String file(String link, int recreate){
                return cham_native_interface.stream_file(link,recreate);
            }
            public static String temp(int recreate){
                return cham_native_interface.stream_temp(recreate);
            }
            public static String encode(String name, String size, String mime, String checksum){
                return cham_native_interface.stream_encode(name,size,mime,checksum);
            }
            public static JSONObject decode(String link){
                try{
                    return new JSONObject(cham_native_interface.stream_decode(link));
                }catch (Exception ignored){
                    return null;
                }
            }

            public interface StreamInterface{
                void onStart(String link);
                void onOpen(String link);
                void onHandshake(String link);
                void onData(String link, long seek);
                void onStop(String link);
            }

        }

        public static class Caster{

            public static int bundle(ChaMNativeInterface.ChaMCasterInterface caster_interface){
                return cham_native_interface.caster_bundle(1,caster_interface);
            }
            public static int tune(String target_id , int tune){
                return cham_native_interface.caster_tune(target_id,tune);
            }
            public static int cast(byte[] data , int size , int end){
                return cham_native_interface.caster_cast(data,size,end);
            }

        }

    }

    public static class Executor{

        private static ExecutorService executor;

        public static void run(){
            if(executor != null){
                shutdown();
            }
            Executor.executor = Executors.newFixedThreadPool(4);
        }

        public static void execute(Runnable runnable){
            executor.execute(runnable);
        }

        public static void shutdown(){
            executor.shutdown();
        }

    }

    public static class Http{

        public static void get(ChaMHttp.ChaMHttpInterface http_interface , String url){
            ChaMHttp http = new ChaMHttp(http_interface,url,null);
            http.execute();
        }

        public static void post(ChaMHttp.ChaMHttpInterface http_interface , String url , String params){
            ChaMHttp http = new ChaMHttp(http_interface,url,params);
            http.execute();
        }

    }

    public static class Info{
        public static String api_path = Environment.getExternalStorageDirectory().getAbsolutePath() + "/ChaM/";
        public static Pair<String,Integer> api_protocol_link = new Pair<>("127.0.0.1",1418);
        public static Pair<String,Integer> api_stream_link = new Pair<>("127.0.0.1",1419);
        public static Pair<String,Integer> api_caster_link = new Pair<>("127.0.0.1",1420);

        public static String preference_name = "ChaM";
    }

    public static class Log{

        public enum LogTag{
            LOG_TAG_UI,
            LOG_TAG_API,
            LOG_TAG_HTTP
        }

        private static String tag_to_string(LogTag tag){
            switch (tag){
                case LOG_TAG_UI:
                    return "ChaM UI Error";
                case LOG_TAG_API:
                    return "ChaM API Error";
                case LOG_TAG_HTTP:
                    return "ChaM HTTP Error";
                default:
                    return "ChaM Error";
            }
        }

        public static void verbose(LogTag tag , String text){
            if(text == null || text.length() <= 0){
                android.util.Log.v(tag_to_string(tag) , "Null Text");
            }else{
                android.util.Log.v(tag_to_string(tag) , text);
            }
        }

        public static void debug(LogTag tag , String text){
            if(text == null || text.length() <= 0){
                android.util.Log.d(tag_to_string(tag) , "Null Text");
            }else{
                android.util.Log.d(tag_to_string(tag) , text);
            }
        }

        public static void info(LogTag tag , String text){
            if(text == null || text.length() <= 0){
                android.util.Log.i(tag_to_string(tag) , "Null Text");
            }else{
                android.util.Log.i(tag_to_string(tag) , text);
            }
        }

        public static void warn(LogTag tag , String text){
            if(text == null || text.length() <= 0){
                android.util.Log.w(tag_to_string(tag) , "Null Text");
            }else{
                android.util.Log.w(tag_to_string(tag) , text);
            }
        }

        public static void error(LogTag tag , String text){
            if(text == null || text.length() <= 0){
                android.util.Log.e(tag_to_string(tag) , "Null Text");
            }else{
                android.util.Log.e(tag_to_string(tag) , text);
            }
        }

    }

    public static class Tools{

        public static int dp_to_px(int dp) {
            DisplayMetrics displayMetrics = context.getResources().getDisplayMetrics();
            return Math.round(dp * (displayMetrics.xdpi / DisplayMetrics.DENSITY_DEFAULT));
        }

        public static int px_to_dp(int px) {
            DisplayMetrics displayMetrics = context.getResources().getDisplayMetrics();
            return Math.round(px / (displayMetrics.xdpi / DisplayMetrics.DENSITY_DEFAULT));
        }

    }

    public static class Time{

        public static String current(){
            return System.currentTimeMillis()+"";
        }

        public static String previews(String chtime){
            return (Long.parseLong(chtime)-1)+"";
        }

        public static String next(String chtime){
            return (Long.parseLong(chtime)+1)+"";
        }

        public static Long number(String chtime){
            return Long.parseLong(chtime);
        }

        public static String chtime(Long number){
            return number+"";
        }

        public static String stdtime(String chtime){
            try{
                int seconds = Integer.parseInt(chtime.split("_")[1]);
                int hour = seconds/3600;
                int min = (seconds%3600)/60;
                String shour = (( hour < 10 ) ? "0"+hour : hour+"");
                String smin = (( min < 10 ) ? "0"+min : min+"");
                return shour+":"+smin;
            }catch (Exception ignored){
                return null;
            }
        }

        public static String profiletime(String chtime){
//            try{
//                long distance_seconds = Long.parseLong(BigInteger.valueOf(System.currentTimeMillis()/1000).subtract(number(chtime).divide(new BigInteger("1000000"))).toString());
//                if(distance_seconds <= 60){
//                    return "1 minute ago";
//                }else if((distance_seconds / 3600) == 0){
//                    return ((int)(distance_seconds / 60))+" minutes ago";
//                }else if((distance_seconds / 3600) <= 24){
//                    return ((int)(distance_seconds / 3600))+" hours ago";
//                }else if((distance_seconds / 86400) <= 30){
//                    return ((int)(distance_seconds / 86400))+" days ago";
//                }else{
//                    return "long time ago";
//                }
//            }catch (Exception ignored){
//                return null;
//            }
            return "";
        }

    }

    public interface ChaMAPIInterface<Type>{
        void onResult(Type result);
    }

}
