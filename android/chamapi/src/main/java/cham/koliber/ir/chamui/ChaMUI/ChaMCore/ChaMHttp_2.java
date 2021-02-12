package cham.koliber.ir.chamui.ChaMUI.ChaMCore;

import android.os.AsyncTask;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.net.HttpURLConnection;
import java.net.URL;

public class ChaMHttp extends AsyncTask<Void,Integer,String>{

    private ChaMHttpInterface http_interface;
    private String url = null;
    private String params = null;


    public ChaMHttp(ChaMHttpInterface http_interface , String url , String params) {
        this.http_interface = http_interface;
        this.url = url;
        this.params = params;
    }

    @Override
    protected void onPreExecute() {
        http_interface.onRequest();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(Void... voids) {
        publishProgress(0);
        String result = null;
        HttpURLConnection connection = null;
        DataOutputStream writer = null;
        DataInputStream reader = null;
        publishProgress(20);
        try {
            connection = (HttpURLConnection) new URL(url).openConnection();
            connection.setReadTimeout(5000);
            connection.setConnectTimeout(10000);
            connection.setDoOutput(true);
            connection.setDoInput(true);
            publishProgress(40);
            if(params != null){
                connection.setRequestMethod("POST");
                writer = new DataOutputStream(connection.getOutputStream());
                writer.write(params.getBytes());
                writer.flush();
            }else{
                connection.setRequestMethod("GET");
            }
            publishProgress(60);
            if(connection.getResponseCode() == HttpURLConnection.HTTP_OK){
                reader = new DataInputStream(connection.getInputStream());
                StringBuilder builder = new StringBuilder();
                byte[] buffer = new byte[1024];
                int read;
                while((read = reader.read(buffer,0,1024)) > 0){
                    builder.append(new String(buffer,0,read));
                }
                result = builder.toString();
            }
            publishProgress(80);
        }catch (Exception ignored) {
            ChaMCoreAPI.Log.error(ChaMCoreAPI.Log.LogTag.LOG_TAG_HTTP,ignored.getMessage());
        }finally {
            try{
                if(connection != null){
                    connection.disconnect();
                }
                if(writer != null){
                    writer.close();
                }
                if(reader != null){
                    reader.close();
                }
            }catch (Exception ignored){}
        }
        publishProgress(100);
        return result;
    }

    @Override
    protected void onProgressUpdate(Integer... values) {
        http_interface.onProgress(values[0]);
        super.onProgressUpdate(values);
    }

    @Override
    protected void onPostExecute(String result) {
        http_interface.onResponse(result);
        super.onPostExecute(result);
    }

    @Override
    protected void onCancelled() {
        http_interface.onResponse(null);
        super.onCancelled();
    }

    public interface ChaMHttpInterface {
        void onRequest();
        void onProgress(int progress);
        void onResponse(String response);
    }

}
