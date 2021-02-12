package ir.koliber.cham;

import android.app.Application;
import android.content.Intent;
import com.vanniktech.emoji.EmojiManager;
import com.vanniktech.emoji.one.EmojiOneProvider;
import ir.koliber.cham.ChaMAPI.ChaMAPI;
import ir.koliber.cham.ChaMAPI.ChaMService.ChaMService;

public class ChaMApplication extends Application{
    @Override
    public void onCreate() {
        // StrictMode.setVmPolicy(new StrictMode.VmPolicy.Builder().detectAll().penaltyLog().penaltyDeath().build());
        startService(new Intent(getApplicationContext(), ChaMService.class));
        super.onCreate();
        EmojiManager.install(new EmojiOneProvider());
        ChaMAPI.run(getApplicationContext());
    }
}
