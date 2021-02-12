package ir.koliber.cham.ChaMUI.ChaMAdapters;

import android.support.v7.widget.RecyclerView;
import android.view.View;
import org.json.JSONObject;
import java.util.Iterator;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMActivity;
import cham.koliber.ir.chamui.ChaMUI.ChaMBase.ChaMAdapter;
import cham.koliber.ir.chamui.ChaMUI.ChaMCore.ChaMCoreAPI;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMFab;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMRecyclerView;
import cham.koliber.ir.chamui.ChaMUI.ChaMUI.ChaMTextView;
import ir.koliber.cham.R;

public class StreamsAdapter extends ChaMAdapter<StreamsAdapter.StreamsAdapterHolder,StreamsAdapter.StreamsAdapterValue>{

    private StreamAdapterType type;

    public StreamsAdapter(ChaMActivity activity, ChaMRecyclerView recycler_view, StreamAdapterType type){
        super(activity,recycler_view,new ChaMAdapterSearcher<StreamsAdapterValue>() {
            @Override
            public boolean search(String search, StreamsAdapterValue streamsAdapterValue) {
                return streamsAdapterValue.getStreamName().toLowerCase().contains(search.toLowerCase());
            }
        },R.layout.adapter_streams);
        this.type = type;
    }

    @Override
    protected StreamsAdapterHolder create_holder(View view) {
        return new StreamsAdapterHolder(view);
    }

    @Override
    protected void init_holder(StreamsAdapterHolder holder, final ChaMAdapterItem<StreamsAdapterValue> item) {
        holder.adapter_streams_textview_name.setText(item.getValue().getStreamName());
        holder.adapter_streams_textview_mime.setText(item.getValue().getStreamMime());
        holder.adapter_streams_fab_stream.showProgress();
        holder.adapter_streams_fab_stream.setProgress((int) (item.getValue().getStreamSeek()*100/item.getValue().getStreamSize()));
        holder.adapter_streams_fab_stream.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                // cancel stream and remove it !!!
                ChaMCoreAPI.Interface.Stream.cancel(item.getValue().getStreamLink());
                remove(item.getKey());
            }
        });
    }

    @Override
    protected void load_holder(ChaMAdapterLoad load) {
        // load from sets and gets
        try{
            JSONObject streams = type == StreamAdapterType.STREAM_ADAPTER_SET ? ChaMCoreAPI.Interface.Stream.sets() : ChaMCoreAPI.Interface.Stream.gets();
            Iterator<String> iterator = streams.keys();
            while(iterator.hasNext()){
                String stream_link = iterator.next();
                String stream_state = streams.getString(stream_link);
                JSONObject stream_object = ChaMCoreAPI.Interface.Stream.decode(stream_link);
                load(load,new StreamsAdapterValue((stream_object != null && stream_object.has("NAME") ? stream_object.getString("NAME") : ""),(stream_object != null && stream_object.has("MIME") ? stream_object.getString("MIME") : ""),stream_link,stream_state));
            }
        }catch (Exception ignored){}
        loader(false);
        end(load);
    }

    class StreamsAdapterHolder extends RecyclerView.ViewHolder{

        private View view;
        private ChaMFab adapter_streams_fab_stream;
        private ChaMTextView adapter_streams_textview_name;
        private ChaMTextView adapter_streams_textview_mime;

        StreamsAdapterHolder(View view) {
            super(view);
            this.view = view;
            this.adapter_streams_fab_stream = view.findViewById(R.id.adapter_streams_fab_stream);
            this.adapter_streams_textview_name = view.findViewById(R.id.adapter_streams_textview_name);
            this.adapter_streams_textview_mime = view.findViewById(R.id.adapter_streams_textview_mime);
        }
    }

    public static class StreamsAdapterValue{

        private String stream_name;
        private String stream_mime;
        private String stream_link;
        private long stream_size;
        private long stream_seek;

        public StreamsAdapterValue(String stream_name, String stream_mime, String stream_link, String stream_state) {
            this.stream_name = stream_name;
            this.stream_mime = stream_mime;
            this.stream_link = stream_link;
            this.stream_size = Long.parseLong(stream_state.split("/")[0]);
            this.stream_seek = Long.parseLong(stream_state.split("/")[1]);
        }

        public String getStreamName() {
            return stream_name;
        }
        public String getStreamMime() {
            return stream_mime;
        }
        public String getStreamLink(){
            return stream_link;
        }
        public long getStreamSize(){
            return stream_size;
        }
        public long getStreamSeek(){
            return stream_seek;
        }

    }

    public enum StreamAdapterType{
        STREAM_ADAPTER_SET,
        STREAM_ADAPTER_GET
    }

}
