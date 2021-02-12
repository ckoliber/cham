package ir.koliber.cham.ChaMAPI.ChaMService;

import android.app.Service;
import android.content.Intent;
import android.os.Binder;
import android.os.IBinder;
import android.support.annotation.Nullable;

public class ChaMService extends Service{

    private final IBinder chBinder = new CHBinder();

//    ChaMService_DataManager dataManager;
//    ChaMService_StreamManager streamManager;
//    ChaMService_SocketManager socketManager;
//    ChaMService_PeerManager peerManager;
//    ChaMService_Protocol socketProtocol;
//    ChaMService_NotificationManager notificationManager;

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return chBinder;
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
//        ChaMService_Protocol.UI.service_set(this);
        startTasks();
        return START_STICKY;
    }

    @Override
    public void onDestroy() {
        stopTasks();
        super.onDestroy();
    }

    private void startTasks(){
//        dataManager = new ChaMService_DataManager(this);
//        socketManager = new ChaMService_SocketManager();
//        streamManager = new ChaMService_StreamManager();
//        notificationManager = new ChaMService_NotificationManager(this);
//        peerManager = new ChaMService_PeerManager();
//        socketProtocol = new ChaMService_Protocol(this,dataManager,socketManager,streamManager,peerManager);
//        dataManager.start_looper();
//        socketManager.start_looper();
//        notificationManager.start_looper();
//        streamManager.start_looper();
//        peerManager.start_looper();
//        dataManager.init_manager(socketManager,notificationManager);
//        socketManager.init_manager(dataManager);
    }

    private void stopTasks(){
//        dataManager.stop_looper();
//        socketManager.stop_looper();
//        notificationManager.stop_looper();
//        streamManager.stop_looper();
//        peerManager.stop_looper();
    }

    public class CHBinder extends Binder {
        public ChaMService getService() {
            return ChaMService.this;
        }
    }

}
