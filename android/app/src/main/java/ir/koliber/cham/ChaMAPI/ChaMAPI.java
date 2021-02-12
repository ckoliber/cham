package ir.koliber.cham.ChaMAPI;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.support.design.widget.Snackbar;
import android.text.Html;
import android.text.Spanned;
import android.util.Pair;
import android.view.View;
import android.view.inputmethod.InputMethodManager;
import com.pes.androidmaterialcolorpickerdialog.ColorPicker;
import com.pes.androidmaterialcolorpickerdialog.ColorPickerCallback;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;

import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMActivity;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import ir.koliber.cham.ChaMUI.ChaMActivities.ProgressActivity;
import ir.koliber.cham.ChaMUI.ChaMAdapters.CountriesAdapter;
import ir.koliber.cham.ChaMUI.ChaMAdapters.LanguagesAdapter;
import ir.koliber.cham.ChaMUI.ChaMAdapters.PeerConnectionsAdapter;
import ir.koliber.cham.ChaMUI.ChaMAlerts.NumberAlert;
import ir.koliber.cham.ChaMUI.ChaMAlerts.TextAlert;
import ir.koliber.cham.ChaMUI.ChaMDialogs.ConnectionCreateDialog;
import ir.koliber.cham.ChaMUI.ChaMDialogs.ConnectionDialog;
import ir.koliber.cham.ChaMUI.ChaMDialogs.ItemsDialog;
import ir.koliber.cham.ChaMUI.ChaMDialogs.PeerCreateDialog;
import ir.koliber.cham.ChaMUI.ChaMDialogs.PeerDialog;
import ir.koliber.cham.ChaMUI.ChaMDialogs.SettingsDialog;
import ir.koliber.cham.ChaMUI.ChaMDialogs.ThemeDialog;
import ir.koliber.cham.R;

public class ChaMAPI {

    @SuppressLint("StaticFieldLeak")
    private static Context context;

    public static void run(Context context){
        ChaMAPI.context = context;
        ChaMCoreAPI.run(context);
    }

    public static class UserInterface {

        public enum TargetType{
            TARGET_TYPE_PEER,
            TARGET_TYPE_CONNECTION
        }

        public static class Chat{

            private static String connection_id;

            public static void open(ChaMActivity activity,String connection_id){
//                chat_connection_id = connection_id;
//                MessengerDetailFragment fragment = new MessengerDetailFragment();
//                fragment.openFragment(activity,connection_id);
            }

            public static void close(ChaMActivity activity){
//                try{
//                    chat_connection_id = null;
//                    for(Fragment fragment : activity.getSupportFragmentManager().getFragments()){
//                        if (fragment instanceof DialogFragment) {
//                            ((DialogFragment) fragment).dismissAllowingStateLoss();
//                        }
//                    }
//                }catch (Exception ignored){}
            }

            public static boolean check(String connection_id){
                try{
                    return Chat.connection_id.equals(connection_id);
                }catch (Exception ignored){
                    return false;
                }
            }

        }

        public static class Permission{

            public static class ConnectionPermission{

                private int link;           // [0:hidden,1:visible,2:visible&change]
                private int name;           // [0:hidden,1:visible,2:visible&change]
                private int bio;            // [0:hidden,1:visible,2:visible&change]
                private int pictures;       // [0:hidden,1:visible,2:visible&add,3:visible&remove,4:visible&add&remove]
                private int peers;          // [0:hidden,1:visible,2:visible&add,3:visible&remove,4:visible&add&remove] // add : boss !!!,have a default peer : * -> default permission for join if have'nt -> can't join !!!
                private int messages;       // [0:noread_nowrite,1:read_nowrite,2:noread_write,3:read_write,4:read_write_delete]

                public ConnectionPermission(String permission){
                    try{
                        this.link = Integer.parseInt(permission.charAt(0)+"");
                        this.name = Integer.parseInt(permission.charAt(1)+"");
                        this.bio = Integer.parseInt(permission.charAt(2)+"");
                        this.pictures = Integer.parseInt(permission.charAt(3)+"");
                        this.peers = Integer.parseInt(permission.charAt(4)+"");
                        this.messages = Integer.parseInt(permission.charAt(5)+"");
                    }catch (Exception ignored){
                        this.link = 0;
                        this.name = 0;
                        this.bio = 0;
                        this.pictures = 0;
                        this.peers = 0;
                        this.messages = 0;
                    }
                }

                public int getLink(){
                    return link;
                }
                public int getName(){
                    return name;
                }
                public int getBio(){
                    return bio;
                }
                public int getPictures(){
                    return pictures;
                }
                public int getPeers(){
                    return peers;
                }
                public int getMessages(){
                    return messages;
                }
                public Drawable getDrawable(){
                    if(getPeers() == 4){
                        return context.getResources().getDrawable(R.drawable.ic_star);
                    }else if(getPeers() > 1){
                        return context.getResources().getDrawable(R.drawable.ic_starhalf);
                    }else{
                        return context.getResources().getDrawable(R.drawable.ic_starclear);
                    }
                }

                @Override
                public String toString() {
                    return link+""+name+""+bio+""+pictures+""+peers+""+messages;
                }
            }

            public static class PeerPermission{

                private int phone;          // [0:hidden,1:visible,2:visible&change]
                private int link;           // [0:hidden,1:visible,2:visible&change]
                private int name;           // [0:hidden,1:visible,2:visible&change]
                private int bio;            // [0:hidden,1:visible,2:visible&change]
                private int state;          // [0:hidden,1:visible,2:visible&change]
                private int pictures;       // [0:hidden,1:visible,2:visible&add,3:visible&remove,4:visible&add&remove]
                private int peers;          // [0:hidden,1:visible,2:visible&add,3:visible&remove,4:visible&add&remove]
                private int connections;    // [0:hidden,1:visible,2:visible&add,3:visible&remove,4:visible&add&remove]

                public PeerPermission(String permission){
                    try{
                        this.phone = Integer.parseInt(permission.charAt(0)+"");
                        this.link = Integer.parseInt(permission.charAt(1)+"");
                        this.name = Integer.parseInt(permission.charAt(2)+"");
                        this.bio = Integer.parseInt(permission.charAt(3)+"");
                        this.state = Integer.parseInt(permission.charAt(4)+"");
                        this.pictures = Integer.parseInt(permission.charAt(5)+"");
                        this.peers = Integer.parseInt(permission.charAt(6)+"");
                        this.connections = Integer.parseInt(permission.charAt(7)+"");
                    }catch (Exception ignored){
                        this.phone = 0;
                        this.link = 0;
                        this.name = 0;
                        this.bio = 0;
                        this.state = 0;
                        this.pictures = 0;
                        this.peers = 0;
                        this.connections = 0;
                    }
                }

                public int getPhone(){
                    return phone;
                }
                public int getLink(){
                    return link;
                }
                public int getName(){
                    return name;
                }
                public int getBio(){
                    return bio;
                }
                public int getState(){
                    return state;
                }
                public int getPictures(){
                    return pictures;
                }
                public int getPeers(){
                    return peers;
                }
                public int getConnections(){
                    return connections;
                }
                public Drawable getDrawable(){
                    if(getPeers() == 4){
                        return context.getResources().getDrawable(R.drawable.ic_star);
                    }else if(getPeers() > 1){
                        return context.getResources().getDrawable(R.drawable.ic_starhalf);
                    }else{
                        return context.getResources().getDrawable(R.drawable.ic_starclear);
                    }
                }

                @Override
                public String toString() {
                    return phone+""+link+""+name+""+bio+""+state+""+pictures+""+peers+""+connections;
                }
            }

            public static PeerPermission peer(String permission){
                return new PeerPermission(permission);
            }

            public static ConnectionPermission connection(String permission){
                return new ConnectionPermission(permission);
            }

        }

        public static class Input{

            public static void color(ChaMActivity activity, int default_color, final ChaMCoreAPI.ChaMAPIInterface<Integer> api_interface){
                ColorPicker colorPicker = new ColorPicker(
                        activity,
                        Color.alpha(default_color),
                        Color.red(default_color),
                        Color.green(default_color),
                        Color.blue(default_color)
                );
                colorPicker.setCallback(new ColorPickerCallback() {
                    @Override
                    public void onColorChosen(int color) {
                        api_interface.onResult(color);
                    }
                });
                colorPicker.show();
            }

            public static void number(ChaMActivity activity, int number_default, int number_min, int number_max, ChaMCoreAPI.ChaMAPIInterface<Integer> api_interface){
                NumberAlert number_alert = new NumberAlert(activity);
                number_alert.open_alert(number_default,number_min,number_max,api_interface);
            }

            public static void text(ChaMActivity activity, String text_default, String text_hint, int text_min, int text_max, boolean text_numeric, ChaMCoreAPI.ChaMAPIInterface<String> api_interface){
                TextAlert text_alert = new TextAlert(activity);
                text_alert.open_alert(text_default,text_hint,text_min,text_max,text_numeric,api_interface);
            }

            public static void confirm(ChaMActivity activity, String text_default, ChaMCoreAPI.ChaMAPIInterface<Boolean> api_interface){

            }

            public static void peer(ChaMActivity activity, ChaMCoreAPI.ChaMAPIInterface<Pair<String,String>> api_interface){
                PeerCreateDialog peercreate_dialog = new PeerCreateDialog();
                peercreate_dialog.open_dialog(activity,api_interface);
            }

            public static void connection(ChaMActivity activity, ChaMCoreAPI.ChaMAPIInterface<String> api_interface){
                ConnectionCreateDialog connectioncreate_dialog = new ConnectionCreateDialog();
                connectioncreate_dialog.open_dialog(activity,api_interface);
            }

        }

        public static class Statistics{

            public static void bandwidth(final ChaMCoreAPI.ChaMAPIInterface<Pair<Long,Long>> api_interface){
                new Thread(new Runnable() {
                    @Override
                    public void run() {
//                        ChaMService_Protocol.Progress.open(Progress.ProgressType.PROGRESS_TYPE_BANDWIDTH , false);
                        try{
                            // start sending
                            long send = -1,recv = -1;
                            for(int cursor = 0 ; cursor < 5 ; cursor++){
//                                send = ChaMService_Protocol.Stream.stream_bandwidth(ChaMService_StreamManager.StreamType.STREAM_TYPE_SET);
                                if(send > 0){
                                    break;
                                }
                            }
                            for(int cursor = 0 ; cursor < 5 ; cursor++){
//                                recv = ChaMService_Protocol.Stream.stream_bandwidth(ChaMService_StreamManager.StreamType.STREAM_TYPE_GET);
                                if(recv > 0){
                                    break;
                                }
                            }
                            api_interface.onResult(new Pair<>(send , recv));
                        }catch (Exception ignored){
                            ChaMCoreAPI.Log.error(ChaMCoreAPI.Log.LogTag.LOG_TAG_UI,ignored.getMessage());
                            api_interface.onResult(null);
                        }
//                        ChaMService_Protocol.Progress.close();
                    }
                }).start();
            }

            public static void cpu(final ChaMCoreAPI.ChaMAPIInterface<Float> api_interface){
                new Thread(new Runnable() {

                    private long getTotalCpuTime(){
                        String[] cpuInfos = null;
                        try{
                            BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream("/proc/stat")), 1000);
                            String load = reader.readLine();
                            reader.close();
                            cpuInfos = load.split(" ");
                        }catch (Exception ignored){}
                        return Long.parseLong(cpuInfos[2]) + Long.parseLong(cpuInfos[3]) + Long.parseLong(cpuInfos[4]) + Long.parseLong(cpuInfos[6]) + Long.parseLong(cpuInfos[5]) + Long.parseLong(cpuInfos[7]) + Long.parseLong(cpuInfos[8]);
                    }

                    private long getAppCpuTime(){
                        String[] cpuInfos = null;
                        try{
                            int pid = android.os.Process.myPid();
                            BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream("/proc/" + pid + "/stat")), 1000);
                            String load = reader.readLine();
                            reader.close();
                            cpuInfos = load.split(" ");
                        }catch(Exception ignored) {}
                        return Long.parseLong(cpuInfos[13]) + Long.parseLong(cpuInfos[14]) + Long.parseLong(cpuInfos[15]) + Long.parseLong(cpuInfos[16]);
                    }

                    @Override
                    public void run() {
//                        ChaMService_Protocol.Progress.open(Progress.ProgressType.PROGRESS_TYPE_CPU , false);
                        try{
                            float totalCpuTime1 = getTotalCpuTime();
                            float processCpuTime1 = getAppCpuTime();
                            Thread.sleep(360);
                            float totalCpuTime2 = getTotalCpuTime();
                            float processCpuTime2 = getAppCpuTime();
                            float cpuRate = 100 * (processCpuTime2 - processCpuTime1) / (totalCpuTime2 - totalCpuTime1);
                            api_interface.onResult(cpuRate);
                        }catch (Exception ignored){
                            ChaMCoreAPI.Log.error(ChaMCoreAPI.Log.LogTag.LOG_TAG_UI,ignored.getMessage());
                            api_interface.onResult(null);
                        }
//                        ChaMService_Protocol.Progress.close();
                    }

                }).start();
            }

        }

        public static class Output{

            public static boolean file(ChaMActivity activity , String link , String mime){
                try{
                    if(mime.contains("cham/theme")){
//                        ThemeDialog themeDialog = new ThemeDialog(link);
//                        themeDialog.show(activity.getSupportFragmentManager(), "Theme");
                    }else{
//                        Uri file_uri;
//                        mime = mime.substring(mime.indexOf("_")+1 , mime.length());
//                        Intent intent = new Intent();
//                        File file = Stream.stream_link(link).link_file;
//                        if(Build.VERSION.SDK_INT >= 24){
//                            file_uri = FileProvider.getUriForFile(activity , "ir.koliber.cham.provider" , file);
//                            intent.addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION);
//                        }else{
//                            file_uri = Uri.fromFile(file);
//                        }
//                        intent.setAction(android.content.Intent.ACTION_VIEW);
//                        intent.setDataAndType(file_uri, mime);
//                        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
//                        activity.startActivity(intent);
                    }
                    return true;
                }catch (Exception ignored){
                    ChaMCoreAPI.Log.error(ChaMCoreAPI.Log.LogTag.LOG_TAG_UI,ignored.getMessage());
                    return false;
                }
            }

            public static void stream(String link){

            }

            public static void peer(ChaMActivity activity, String peer_id){
                PeerDialog peer_dialog = new PeerDialog();
                peer_dialog.open_dialog(activity,peer_id);
            }

            public static void connection(ChaMActivity activity, String connection_id){
                ConnectionDialog connection_dialog = new ConnectionDialog();
                connection_dialog.open_dialog(activity,connection_id);
            }

            public static void snack(View view, ChaMActivity activity, View root_view, String text){
                // activities ->    root_view == activity.getCurrentFocus()
                // fragments ->     root_view == getView()
                try{
                    InputMethodManager inputManager = (InputMethodManager) activity.getSystemService(Context.INPUT_METHOD_SERVICE);
                    if (inputManager != null) {
                        inputManager.hideSoftInputFromWindow(root_view.getWindowToken(),0);
                    }
                }catch (Exception ignored){}
                Snackbar.make(view, Html.fromHtml("<font color=\"#ffffff\">"+text+"</font>"),Snackbar.LENGTH_LONG).setActionTextColor(Color.WHITE).show();
            }

            public static void profile(String id, TargetType type){

            }

            public static void permission(String permission, boolean edittable, ChaMCoreAPI.ChaMAPIInterface<String> api_interface){

            }

            public static void peers(String id, TargetType type){

            }

            public static void connections(String id){

            }

            public static void pictures(String id, TargetType type){

            }

            public static void settings(ChaMActivity activity){
                SettingsDialog settings_dialog = new SettingsDialog();
                settings_dialog.open_dialog(activity);
            }

            public static void theme(ChaMActivity activity, String link){
                ThemeDialog theme_dialog = new ThemeDialog();
                theme_dialog.open_dialog(activity,link);
            }

        }

        public static class Items{

            public static void language(ChaMActivity activity, final ChaMCoreAPI.ChaMAPIInterface<LanguagesAdapter.LanguagesAdapterValue> api_interface){
                ItemsDialog items_dialog = new ItemsDialog();
                items_dialog.open_dialog(ItemsDialog.ItemsDialogType.ITEMS_DIALOG_LANGUAGES, activity, 0, new ChaMCoreAPI.ChaMAPIInterface() {
                    @Override
                    public void onResult(Object result) {
                        api_interface.onResult((LanguagesAdapter.LanguagesAdapterValue) result);
                    }
                });
            }

            public static void countries(ChaMActivity activity, final ChaMCoreAPI.ChaMAPIInterface<CountriesAdapter.CountriesAdapterValue> api_interface){
                ItemsDialog items_dialog = new ItemsDialog();
                items_dialog.open_dialog(ItemsDialog.ItemsDialogType.ITEMS_DIALOG_COUNTRIES, activity, 0, new ChaMCoreAPI.ChaMAPIInterface() {
                    @Override
                    public void onResult(Object result) {
                        api_interface.onResult((CountriesAdapter.CountriesAdapterValue) result);
                    }
                });
            }

            public static void streams(ChaMActivity activity, boolean set_streams){
                ItemsDialog items_dialog = new ItemsDialog();
                items_dialog.open_dialog(set_streams ? ItemsDialog.ItemsDialogType.ITEMS_DIALOG_STREAMS_SET : ItemsDialog.ItemsDialogType.ITEMS_DIALOG_STREAMS_GET, activity);
            }

            public static void mimes(ChaMActivity activity, boolean set_mimes){
                ItemsDialog items_dialog = new ItemsDialog();
                items_dialog.open_dialog(set_mimes ? ItemsDialog.ItemsDialogType.ITEMS_DIALOG_MIMES_SET : ItemsDialog.ItemsDialogType.ITEMS_DIALOG_MIMES_GET, activity);
            }

            public static void connection_peers(ChaMActivity activity, String connection_id, String self_permission){
                ItemsDialog items_dialog = new ItemsDialog();
                items_dialog.open_dialog(ItemsDialog.ItemsDialogType.ITEMS_DIALOG_CONNECTION_PEERS, activity, connection_id, self_permission);
            }

            public static void peer_peers(ChaMActivity activity, String peer_id, String self_permission){
                ItemsDialog items_dialog = new ItemsDialog();
                items_dialog.open_dialog(ItemsDialog.ItemsDialogType.ITEMS_DIALOG_PEER_PEERS, activity, peer_id, self_permission);
            }

            public static void peer_connections(ChaMActivity activity, String peer_id, String self_permission){
                ItemsDialog items_dialog = new ItemsDialog();
                items_dialog.open_dialog(ItemsDialog.ItemsDialogType.ITEMS_DIALOG_PEER_CONNECTIONS, activity, peer_id, self_permission);
            }

            public static void select_connections(ChaMActivity activity, boolean multi_select, ChaMCoreAPI.ChaMAPIInterface<ArrayList<PeerConnectionsAdapter.PeerConnectionsAdapterValue>> api_interface){
                ItemsDialog items_dialog = new ItemsDialog();
                items_dialog.open_dialog(ItemsDialog.ItemsDialogType.ITEMS_DIALOG_PEER_CONNECTIONS, activity, multi_select ? 1 : 0,api_interface);
            }

        }

        public static class Do{

            public static void restart(ChaMActivity activity, Class restart_activity){
                activity.startActivity(new Intent(activity , restart_activity));
                activity.finish();
            }

            public static void update(final String link){
//                ChaMService_Protocol.Progress.open(Progress.ProgressType.PROGRESS_TYPE_UPDATE , false);
//                stream_toggle(link , ChaMService_StreamManager.StreamType.STREAM_TYPE_GET , ChaMService_StreamManager.StreamForce.STREAM_FORCE_FORCE_START);
//                stream_listener(link, new ChaMServiceStreamListener() {
//                    private long size = -1;
//                    private long buffer = -1;
//                    @Override
//                    public void onPipe() {}
//
//                    @Override
//                    public void onData(long size , long buffer) {
//                        this.size = size;
//                        this.buffer = buffer;
//                        ChaMService_Protocol.Progress.update((int)((buffer*100)/size));
//                    }
//
//                    @Override
//                    public void onUnpipe() {
//                        ChaMService_Protocol.Progress.close();
//                        if(buffer >= size){
//                            // install app
//                            Stream.StreamLink stream_link = stream_link(link);
//                            File run_file = new File(stream_link.link_file.getPath()+".apk");
//                            stream_link.link_file.renameTo(run_file);
//                            Intent intent = new Intent(Intent.ACTION_VIEW);
//                            intent.setDataAndType(Uri.fromFile(run_file), "application/vnd.android.package-archive");
//                            intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
//                            service.startActivity(intent);
//                        }
//                    }
//                });
            }

            public static void copy(String file_1, String file_2){

            }

            public static String mmap(String file_path){
                try{
                    StringBuilder result = new StringBuilder();
                    FileInputStream reader = new FileInputStream(file_path);
                    byte[] data = new byte[1024];
                    int read;
                    while((read = reader.read(data,0,1024)) > 0){
                        result.append(new String(data, 0, read));
                    }
                    return result.toString();
                }catch (Exception ignored){
                    return null;
                }
            }

            public static ArrayList<CountriesAdapter.CountriesAdapterValue> countries(){
                ArrayList<CountriesAdapter.CountriesAdapterValue> result = new ArrayList<>();
                InputStreamReader reader = null;
                try{
                    reader = new InputStreamReader(context.getAssets().open("countries.txt"));
                    final BufferedReader bufferedReader = new BufferedReader(reader);
                    String line;
                    while((line = bufferedReader.readLine()) != null){
                        result.add(new CountriesAdapter.CountriesAdapterValue(line.split(";")[2],line.split(";")[1],line.split(";")[0],(line.split(";").length > 3 ? line.split(";")[3] : "")));
                    }
                }catch (Exception ignored){
                    ChaMCoreAPI.Log.error(ChaMCoreAPI.Log.LogTag.LOG_TAG_UI,ignored.getMessage());
                }finally{
                    try {
                        if(reader != null){
                            reader.close();
                        }
                    }catch (Exception ignored){}
                }
                return result;
            }

            public static ArrayList<LanguagesAdapter.LanguagesAdapterValue> languages(){
                ArrayList<LanguagesAdapter.LanguagesAdapterValue> result = new ArrayList<>();
                result.add(new LanguagesAdapter.LanguagesAdapterValue("English","EN"));
                result.add(new LanguagesAdapter.LanguagesAdapterValue("فارسی","FA"));
                return result;
            }

        }
    }

    public static class Text{

        public static String get_word(String text,int offset){
            try{
                if(text.charAt(offset) != ' '){
                    String before_text = text.substring(0,offset);
                    String after_text = text.substring(offset,text.length());
                    int before_space = before_text.lastIndexOf(" ");
                    int before_line = before_text.lastIndexOf("\n");
                    int after_space = after_text.indexOf(" ");
                    int after_line = after_text.indexOf("\n");
                    int first,last;
                    if(before_space == -1){
                        if(before_line == -1){
                            first = 0;
                        }else{
                            first = before_line;
                        }
                    }else{
                        if(before_line == -1){
                            first = before_space;
                        }else{
                            first = before_space > before_line ? before_space : before_line;
                        }
                    }
                    if(after_space == -1){
                        if(after_line == -1){
                            last = text.length();
                        }else{
                            last = offset+after_line;
                        }
                    }else{
                        if(after_line == -1){
                            last = offset+after_space;
                        }else{
                            last = offset+(after_space > after_line ? after_line : after_space);
                        }
                    }
                    return text.substring(first,last).replaceAll(" ","").replaceAll("\n","");
                }
            }catch (Exception ignored){}
            return null;
        }

        public static int get_word_type(String word){
            String http_regex = "^(https?:\\/\\/)?([\\da-z\\.-]+)\\.([a-z\\.]{2,6})([\\/\\w \\.-]*)*\\/?$";
            String cham_regex = "^cham:\\/\\/(GPT|CHT)\\/([a-zA-Z0-9]{8})-([a-zA-Z0-9]{4})-([a-zA-Z0-9]{4})-([a-zA-Z0-9]{4})-([a-zA-Z0-9]{12})\\/$";
            String mail_regex = "^([a-z0-9_\\.-]+)@([\\da-z\\.-]+)\\.([a-z\\.]{2,6})$";
            String ip_regex = "^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$";
            String fid_regex = "^@.*";
            // 0 -> nothing
            // 1 -> https? link
            // 2 -> cham link
            // 3 -> mail link
            // 4 -> ip link
            if(word.matches(http_regex)){
                return 1;
            }else if(word.matches(cham_regex)){
                return 2;
            }else if(word.matches(mail_regex)){
                return 3;
            }else if(word.matches(ip_regex)){
                return 4;
            }else if(word.matches(fid_regex)){
                return 5;
            }else{
                return 0;
            }
        }

        private static String word_to_spanned(String word){
            if(get_word_type(word) > 0){
                return "<a href=\""+word+"\">"+word+"</a>";
            }else{
                return word;
            }
        }

        public static Spanned get_text_view(String text){
            String text_result = "";
            for(String line : text.split("\n")){
                for(String word : line.split(" ")){
                    text_result+=word_to_spanned(word)+" ";
                }
                text_result = text_result.substring(0,text_result.length()-1);
                text_result += "<br/>";
            }
            text_result = text_result.substring(0,text_result.lastIndexOf("<br/>"));
            return Html.fromHtml(text_result);
        }

        public static String size(long byte_size){
            if(byte_size < 1024){
                return byte_size+" B";
            }else{
                if(byte_size < 1048576) {
                    return ((int)byte_size/1024)+" KB";
                }else{
                    if(byte_size < 1073741824){
                        return ((int)byte_size/1048576)+" MB";
                    }else{
                        return ((int)byte_size/1073741824)+" GB";
                    }
                }
            }
        }

    }

    public static class Progress{

        private static ProgressActivity.ProgressInterface progress_interface;
        private static boolean close = false;

        public enum ProgressType{
            PROGRESS_TYPE_FILE,
            PROGRESS_TYPE_COPY,
            PROGRESS_TYPE_BANDWIDTH,
            PROGRESS_TYPE_CPU,
            PROGRESS_TYPE_UPDATE
        }

        public static void setProgressInterface(ProgressActivity.ProgressInterface progress_interface){
            Progress.progress_interface = progress_interface;
            if(Progress.close){
                finish();
            }
        }

        public static void open(ProgressType type , boolean dismissable){
            finish();
            Progress.close = false;
            Intent intent = new Intent(context , ProgressActivity.class);
            String text = "";
            switch (type){
                case PROGRESS_TYPE_FILE:
//                    text = context.getText(R.string.activity_progress_file).toString();
                    break;
                case PROGRESS_TYPE_COPY:
//                    text = context.getText(R.string.activity_progress_copy).toString();
                    break;
                case PROGRESS_TYPE_BANDWIDTH:
//                    text = context.getText(R.string.activity_progress_bandwidth).toString();
                    break;
                case PROGRESS_TYPE_CPU:
//                    text = context.getText(R.string.activity_progress_cpu).toString();
                    break;
                case PROGRESS_TYPE_UPDATE:
//                    text = context.getText(R.string.activity_progress_update).toString();
                    break;
            }
            intent.putExtra("PROGRESS_TEXT" , text);
            intent.putExtra("PROGRESS_DISMISSABLE" , dismissable);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            context.startActivity(intent);
        }

        public static void progress(int progress){
            if(progress_interface != null){
                progress_interface.progress(progress);
            }
        }

        public static void finish(){
            if(progress_interface != null){
                progress_interface.finish();
            }else{
                Progress.close = true;
            }
        }
    }

}
