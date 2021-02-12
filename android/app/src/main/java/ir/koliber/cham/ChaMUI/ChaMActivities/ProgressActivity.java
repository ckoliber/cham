package ir.koliber.cham.ChaMUI.ChaMActivities;

import android.view.MotionEvent;
import android.view.View;
import android.view.Window;
import android.view.WindowManager;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMActivity;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMProgress;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMTextView;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMToolbar;
import ir.koliber.cham.ChaMAPI.ChaMAPI;
import ir.koliber.cham.R;

public class ProgressActivity extends ChaMActivity{

    private boolean dismissable;

    private ChaMToolbar activity_progress_toolbar;
    private ChaMTextView activity_progress_textview;
    private ChaMProgress activity_progress_progress;

    @Override
    public void init_activity() {
        this.dismissable = getIntent().getBooleanExtra("PROGRESS_DISMISSABLE" , true);
        requestWindowFeature(Window.FEATURE_NO_TITLE);
        ChaMAPI.Progress.setProgressInterface(new ProgressInterface());
        getWindow().setFlags(WindowManager.LayoutParams.FLAG_NOT_TOUCH_MODAL, WindowManager.LayoutParams.FLAG_NOT_TOUCH_MODAL);
        getWindow().setFlags(WindowManager.LayoutParams.FLAG_WATCH_OUTSIDE_TOUCH, WindowManager.LayoutParams.FLAG_WATCH_OUTSIDE_TOUCH);
        setTheme(R.style.ChaM_Base_ChaMProgress);
        setContentView(R.layout.activity_progress);
        init_variables();
        init_toolbar();
        init_content();
    }

    @Override
    public void start_activity() {

    }

    private void init_variables(){
        activity_progress_toolbar = findViewById(R.id.activity_progress_toolbar);
        activity_progress_textview = findViewById(R.id.activity_progress_textview);
        activity_progress_progress = findViewById(R.id.activity_progress_progress);
    }

    private void init_toolbar(){
        if(dismissable){
            activity_progress_toolbar.setNavigationIcon(getChaMActivity().getResources().getDrawable(R.drawable.ic_back));
            activity_progress_toolbar.setNavigationOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    finish();
                }
            });
        }
    }

    private void init_content(){
        activity_progress_textview.setText(getIntent().getStringExtra("PROGRESS_TEXT"));
    }

    @Override
    public boolean onTouchEvent(MotionEvent event) {
        if (MotionEvent.ACTION_OUTSIDE == event.getAction()) {
            if(dismissable){
                finish();
                return true;
            }
        }
        return super.onTouchEvent(event);
    }

    @Override
    public void onBackPressed() {
        if(dismissable){
            finish();
        }
    }

    public class ProgressInterface{

        public void progress(final int progress){
            getChaMActivity().runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    activity_progress_progress.setIndeterminate(false);
                    activity_progress_progress.setProgress(progress);
                }
            });
        }

        public void finish(){
            getChaMActivity().finish();
        }
    }

}
