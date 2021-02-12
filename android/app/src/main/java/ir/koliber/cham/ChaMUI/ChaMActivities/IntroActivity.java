package ir.koliber.cham.ChaMUI.ChaMActivities;

import android.os.Bundle;
import com.github.paolorotolo.appintro.AppIntro;
import com.github.paolorotolo.appintro.AppIntroFragment;
import ir.koliber.cham.R;

public class IntroActivity extends AppIntro{

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        init_activity();
    }

    private void init_activity(){
        addSlide(AppIntroFragment.newInstance("TITLE","TEXT", R.drawable.emoji_backspace,getResources().getColor(R.color.cardview_shadow_end_color)));
    }

}
