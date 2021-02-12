package cham.koliber.ir.chamui.ChaMUI.ChaMCore;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class ChaMPool {
    private int pool_size;
    private Thread[] pool;
    private BlockingQueue<Runnable> queue;

    public ChaMPool(int pool_size){
        this.pool_size = pool_size;
        this.pool = new Thread[pool_size];
        this.queue = new LinkedBlockingQueue<>(128);
        for(int cursor = 0 ; cursor < pool_size ; cursor++){
            pool[cursor] = new Thread(new Runnable() {
                @Override
                public void run() {
                    try{
                        while(true){
                            queue.take().run();
                        }
                    }catch(Exception ignored) {
                        // log
                    }
                }
            });
        }
    }

    public void start(){
        for(int cursor = 0 ; cursor < pool_size ; cursor++){
            pool[cursor].start();
        }
    }

    public void post(Runnable runnable){
        try {
            queue.put(runnable);
        } catch (Exception ignored) {
            // log
        }
    }

    public void stop(){
        for(int cursor = 0 ; cursor < pool_size ; cursor++){
            try {
                pool[cursor].interrupt();
            }catch (Exception ignored){}
        }
    }
}
