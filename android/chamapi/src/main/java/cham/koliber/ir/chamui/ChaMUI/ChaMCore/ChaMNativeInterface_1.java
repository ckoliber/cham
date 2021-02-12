package cham.koliber.ir.chamui.ChaMUI.ChaMCore;

public class ChaMNativeInterface {

    // PUBLIC JINTERFACES :
    //      0 : onInterface
    //      1 : onCaster
    //      2 : onProtocol

    static {
        System.loadLibrary("ChaM");
    }

    public void construct(String self_id){
        interface_new(
                ChaMCoreAPI.Info.api_protocol_link.first,
                ChaMCoreAPI.Info.api_protocol_link.second,
                0,
                ChaMCoreAPI.Info.api_stream_link.first,
                ChaMCoreAPI.Info.api_stream_link.second,
                1,
                ChaMCoreAPI.Info.api_caster_link.first,
                ChaMCoreAPI.Info.api_caster_link.second,
                1,
                ChaMCoreAPI.Info.api_path,
                self_id
        );
    }
    public void destruct(){
        interface_free();
    }

    // INTERFACE API
    private native void interface_new(String protocol_host,int protocol_port,int stream_parallelism,String stream_host,int stream_port,int stream_pool,String caster_host,int caster_port,int caster_pool,String interface_path,String self_id);
    private native void interface_free();
    public native int interface_start();
    public native int interface_state();
    public native int interface_stop();

    public native int interface_bundle(int index, ChaMInterfaceInterface interface_interface);
    public native int interface_self_set(String key , String value);
    public native String interface_self_get(String key);

    // PROTOCOL API
    public native int protocol_bundle(int index, ChaMProtocolInterface protocol_interface);
    public native int protocol_update(String token, ChaMProtocolInterface protocol_interface);
    public native void protocol_peer_captcha(ChaMProtocolInterface protocol_interface, String token, String phone, String type);
    public native void protocol_peer_login(ChaMProtocolInterface protocol_interface, String token, String id, String scode, String platform, String version);
    public native void protocol_peer_logout(ChaMProtocolInterface protocol_interface, String token);
    public native void protocol_peer_create(ChaMProtocolInterface protocol_interface, String token, String phone, String captcha);
    public native void protocol_peer_delete(ChaMProtocolInterface protocol_interface, String token);
    public native void protocol_peer_load(ChaMProtocolInterface protocol_interface, String token, String id, String type, String mode, String param_1, String param_2, int local, int trying);
    public native void protocol_peer_set(ChaMProtocolInterface protocol_interface, String token, String id, String key, String option, String value, String operator);
    public native void protocol_peer_id(ChaMProtocolInterface protocol_interface, String token, String key, String type);
    public native void protocol_connection_create(ChaMProtocolInterface protocol_interface, String token);
    public native void protocol_connection_delete(ChaMProtocolInterface protocol_interface, String token, String id);
    public native void protocol_connection_set(ChaMProtocolInterface protocol_interface, String token, String id, String key, String option, String value, String operator);
    public native void protocol_connection_id(ChaMProtocolInterface protocol_interface, String token, String key, String type);
    public native void protocol_message_set(ChaMProtocolInterface protocol_interface, String token, String conection_id, String message_type, String forward_id, String reply_date, String data_1, String data_2);
    public native void protocol_message_handle(ChaMProtocolInterface protocol_interface, String token, String conection_id, String handle);

    public native int stream_update(String link, ChaMStreamInterface stream_interface);
    public native void stream_set(ChaMStreamInterface stream_interface , String link , String path , int calltime);
    public native void stream_get(ChaMStreamInterface stream_interface , String link , String path , int calltime);
    public native String stream_sets();
    public native String stream_gets();
    public native int stream_cancel(String link);
    public native String stream_state(String link);
    public native int stream_clear();
    public native long stream_size();
    public native String stream_getdata(String link);
    public native int stream_setdata(String link , String state);
    public native String stream_file(String link , int recreate);
    public native String stream_temp(int recreate);
    public native String stream_encode(String name , String size , String mime , String checksum);
    public native String stream_decode(String link);

    public native int caster_bundle(int index, ChaMCasterInterface caster_interface);
    public native int caster_tune(String target_id , int tune);
    public native int caster_cast(byte[] data , int size , int end);

    public interface ChaMProtocolInterface {
        void onMessage(String token, String request, String response);
    }
    public interface ChaMStreamInterface {
        void onStart(String link);
        void onOpen(String link);
        void onHandshake(String link);
        void onData(String link, long seek);
        void onStop(String link);
    }
    public interface ChaMCasterInterface {
        void onTune(String target_id, int tune);
        void onCast(String target_id, byte[] data, int size);
    }
    public interface ChaMInterfaceInterface{
        void onAppRestart();
        void onAppUpgrade(String message);
        void onAppHandshake();
        void onAppOnline();
        void onAppOffline();
        void onStreamSet(String message);
        void onStreamGet(String message);
    }

}
