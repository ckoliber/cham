#include "stream.h"

typedef struct Stream_{
    struct Stream interface;
    StreamParallelism parallelism;
    int buffer_size;
    char *stream_host;
    int stream_port;
    void (*onStart)(struct Stream *stream , char *link);
    void (*onOpen)(struct Stream *stream , char *link);
    void (*onHandshake)(struct Stream *stream , char *link);
    void (*onData)(struct Stream *stream , char *link , long seek);
    void (*onStop)(struct Stream *stream , char *link);
    char *id;
    char *scode;
    Kmap *kmap;
    Ktcp *ktcp;
    Kmsg *kmsg;
    Ksem *ksem;
    Kprocessor *kprocessor;
} Stream_;

void* stream_looper(Stream *stream);
int stream_handshake(Ktcp *ktcp , KtcpPeer *kpeer , char *data , char *link , StreamPipe *pipe);
void stream_onSpawn(struct Ktcp *ktcp);
void stream_onOpen(struct Ktcp *ktcp , KtcpPeer *kpeer , void *arg);
void stream_onReadable(struct Ktcp *ktcp , KtcpPeer *kpeer , char *data , int size);
void stream_onWritable(struct Ktcp *ktcp , KtcpPeer *kpeer);
void stream_onClose(struct Ktcp *ktcp , KtcpPeer *kpeer);

int stream_key_comparator(void *key_1 , void *key_2);
void stream_key_destructor(void *key);
void stream_value_destructor(void *value);

int stream_start(struct Stream *stream , char *setting_id , char *setting_scode);
int stream_set(struct Stream *stream , char *link , char *path , int calltime);
int stream_get(struct Stream *stream , char *link , char *path , int calltime);
Kmap* stream_sets(struct Stream *stream);
Kmap* stream_gets(struct Stream *stream);
int stream_cancel(struct Stream *stream , char *link);
StreamPipe* stream_state(struct Stream *stream , char *link);
int stream_stop(struct Stream *stream);
char *stream_encode(struct Stream *stream , Kmap *kmap);
Kmap *stream_decode(struct Stream *stream , char *cipher);

void* stream_looper(Stream *stream){
    Stream_ *stream_ = (Stream_ *) stream;
    if(stream_ == NULL){
        return NULL;
    }
    while(1){
        if(stream_->parallelism == STREAM_PARALLELISM_SEQUENTIAL){
            stream_->ksem->wait(stream_->ksem);
        }
        char *link = stream_->kmsg->poll(stream_->kmsg);
        if(stream_->kmap->indexof(stream_->kmap,link) >= 0){
            if(!stream_->ktcp->connect(stream_->ktcp,link,stream_->stream_host,stream_->stream_port)){
                // not connect -> onClose
                stream_->ksem->post(stream_->ksem);
                stream_->kmap->remove(stream_->kmap,link);
                stream_->onData(stream,link,-1);
                stream_->onStop(stream,link);
            }
        }
        kmemory_free(link);
    }
    return NULL;
}
int stream_handshake(Ktcp *ktcp , KtcpPeer *kpeer , char *data , char *link , StreamPipe *pipe){
    Kmap *handshake_map = kson_parse(data);
    if(handshake_map == NULL){
        return 0;
    }
    if(handshake_map->indexof(handshake_map , "SEEK") < 0){
        kmap_free(handshake_map);
        return 0;
    }
    pipe->seek = atol(handshake_map->get(handshake_map , "SEEK"));
    pipe->fd = open(pipe->path , O_RDWR);
    if(lseek(pipe->fd , pipe->seek , SEEK_SET) < 0){
        close(pipe->fd);
        kmap_free(handshake_map);
        return 0;
    }
    pipe->state = STREAM_PIPE_STATE_HANDSHAKE;
    ((Stream_ *)ktcp->bundle)->onHandshake(((Stream *)ktcp->bundle),link);
    if(pipe->type == STREAM_PIPE_TYPE_SET){
        ktcp->tune(ktcp,kpeer,KTCP_TUNE_WRONLY);
    }else{
        ktcp->tune(ktcp,kpeer,KTCP_TUNE_RDONLY);
    }
    kmap_free(handshake_map);
    return 1;
}
void stream_onSpawn(struct Ktcp *ktcp){}
void stream_onOpen(struct Ktcp *ktcp , KtcpPeer *kpeer , void *arg){
    Stream_ *stream_ = ktcp->bundle;
    if(stream_ == NULL){
        return;
    }
    char *link = arg;
    ktcp->tune(ktcp,kpeer,KTCP_TUNE_RDONLY);
    StreamPipe *pipe = stream_->kmap->get(stream_->kmap , link);
    if(pipe == NULL){
        return;
    }
    // onOpen + change state peer
    pipe->state = STREAM_PIPE_STATE_OPEN;
    stream_->onOpen((struct Stream *) stream_, link);
    Kmap *handshake_map = kson_parse("{}");
    handshake_map->put(handshake_map,"STREAM",KTYPE_STACK,pipe->type == STREAM_PIPE_TYPE_GET ? "GET" : "SET",KTYPE_STACK);
    handshake_map->put(handshake_map,"ID",KTYPE_STACK,stream_->id,KTYPE_STACK);
    handshake_map->put(handshake_map,"SCODE",KTYPE_STACK,stream_->scode,KTYPE_STACK);
    handshake_map->put(handshake_map,"LINK",KTYPE_STACK,pipe->link,KTYPE_STACK);
    int fd = open(pipe->path , O_RDWR);
    pipe->seek = lseek(fd , 0 , SEEK_END);
    close(fd);
    if(pipe->seek < 0){
        // close socket -> onClose
        ktcp->close(ktcp,kpeer);
        kmap_free(handshake_map);
        return;
    }
    handshake_map->put(handshake_map,"SEEK",KTYPE_STACK,kmemory_copy_long(pipe->seek),KTYPE_HEAP);
    char *handshake_message = kson_pack(handshake_map);
    kmap_free(handshake_map);
    ktcp->send(ktcp, kpeer, handshake_message, (int) strlen(handshake_message));
    kmemory_free(handshake_message);
}
void stream_onReadable(struct Ktcp *ktcp , KtcpPeer *kpeer , char *data , int size){
    Stream_ *stream_ = ktcp->bundle;
    if(stream_ == NULL){
        return;
    }
    char *link = NULL;
    StreamPipe *pipe = NULL;
    for(int cursor = 0 ; cursor < stream_->kmap->length(stream_->kmap) ; cursor++){
        link = stream_->kmap->getkey(stream_->kmap , cursor);
        pipe = stream_->kmap->getvalue(stream_->kmap , cursor);
        if(pipe != NULL && pipe->kpeer->fd == kpeer->fd && pipe->kpeer->index == kpeer->index){ break; }
    }
    if(link == NULL || pipe == NULL){ return; }
    if(pipe->state == STREAM_PIPE_STATE_HANDSHAKE > 0){
        // data -> write to file
        int data_size = (int) write(pipe->fd , data , (size_t) size);
        if(data_size > 0){
            pipe->seek += data_size;
            pipe->callback = (pipe->callback + 1)%pipe->calltime;
            if(pipe->callback == 0){
                stream_->onData((struct Stream *) stream_, link, pipe->seek);
            }
        }else{
            ktcp->close(ktcp,kpeer);
        }
    }else{
        // handshake message -> response
        if(!stream_handshake(ktcp,kpeer,data,link,pipe)){
            ktcp->close(ktcp,kpeer);
        }
    }
}
void stream_onWritable(struct Ktcp *ktcp , KtcpPeer *kpeer){
    if(kpeer == NULL){
        return;
    }
    Stream_ *stream_ = ktcp->bundle;
    if(stream_ == NULL){
        return;
    }
    char *link = NULL;
    StreamPipe *pipe = NULL;
    for(int cursor = 0 ; cursor < stream_->kmap->length(stream_->kmap) ; cursor++){
        link = stream_->kmap->getkey(stream_->kmap , cursor);
        pipe = stream_->kmap->getvalue(stream_->kmap , cursor);
        if(pipe != NULL && pipe->kpeer->fd == kpeer->fd && pipe->kpeer->index == kpeer->index){ break; }
    }
    if(link == NULL || pipe == NULL){ return; }
    // data -> read from file
    void *buffer = kmemory_alloc((size_t) stream_->buffer_size);
    int buffer_size = (int) read(pipe->fd , buffer , (size_t) stream_->buffer_size);
    if(buffer_size > 0){
        buffer_size = ktcp->send(ktcp,kpeer,buffer,buffer_size);
        pipe->seek += buffer_size;
        pipe->callback = (pipe->callback + 1)%pipe->calltime;
        if(pipe->callback == 0){
            stream_->onData((struct Stream *) stream_, link, pipe->seek);
        }
    }else{
        ktcp->close(ktcp,kpeer);
    }
    kmemory_free(buffer);
}
void stream_onClose(struct Ktcp *ktcp , KtcpPeer *kpeer){
    Stream_ *stream_ = ktcp->bundle;
    if(stream_ == NULL){
        return;
    }
    for(int cursor = 0 ; cursor < stream_->kmap->length(stream_->kmap) ; cursor++){
        StreamPipe *pipe = stream_->kmap->getvalue(stream_->kmap , cursor);
        if(pipe != NULL && pipe->kpeer->fd == kpeer->fd && pipe->kpeer->index == kpeer->index){
            stream_->kmap->remove(stream_->kmap,stream_->kmap->getkey(stream_->kmap,cursor));
            stream_->onData((struct Stream *) stream_, stream_->kmap->getkey(stream_->kmap, cursor), pipe->seek);
            stream_->onStop((struct Stream *) stream_, stream_->kmap->getkey(stream_->kmap, cursor));
            break;
        }
    }
    stream_->ksem->post(stream_->ksem);
}
int stream_key_comparator(void *key_1 , void *key_2){
    return strcmp(key_1 , key_2) == 0;
}
void stream_key_destructor(void *key){
    kmemory_free(key);
}
void stream_value_destructor(void *value){
    kmemory_free(((StreamPipe*)value)->link);
    kmemory_free(((StreamPipe*)value)->path);
    kmemory_free(value);
}

int stream_start(struct Stream *stream , char *setting_id , char *setting_scode){
    Stream_ *stream_ = (Stream_ *) stream;
    if(stream_ == NULL){
        return 0;
    }
    if(stream_->id != NULL || stream_->scode != NULL){
        return 0;
    }
    stream_->id = kmemory_copy_string(setting_id);
    stream_->scode = kmemory_copy_string(setting_scode);
    stream_->kprocessor->start(stream_->kprocessor);
    stream_->ktcp->connectivity(stream_->ktcp,NULL);
    stream_->kprocessor->post(stream_->kprocessor, (void *(*)(void *)) stream_looper, stream);
    return 1;
}
int stream_set(struct Stream *stream , char *link , char *path , int calltime){
    Stream_ *stream_ = (Stream_ *) stream;
    if(stream_ == NULL){
        return 0;
    }
    StreamPipe *pipe = kmemory_alloc(sizeof(StreamPipe));
    pipe->type = STREAM_PIPE_TYPE_SET;
    pipe->state = STREAM_PIPE_STATE_START;
    pipe->link = kmemory_copy_string(link);
    pipe->path = kmemory_copy_string(path);
    pipe->calltime = calltime;
    pipe->callback = -1;
    pipe->kpeer = NULL;
    pipe->size = -1;
    pipe->seek = -1;
    pipe->fd = -1;
    stream_->kmap->put(stream_->kmap,kmemory_copy_string(link),KTYPE_HEAP,pipe,KTYPE_HEAP);
    stream_->kmsg->push(stream_->kmsg,kmemory_copy_string(link));
    stream_->onStart(stream,link);
    return 1;
}
int stream_get(struct Stream *stream , char *link , char *path , int calltime){
    Stream_ *stream_ = (Stream_ *) stream;
    if(stream_ == NULL){
        return 0;
    }
    StreamPipe *pipe = kmemory_alloc(sizeof(StreamPipe));
    pipe->type = STREAM_PIPE_TYPE_GET;
    pipe->state = STREAM_PIPE_STATE_START;
    pipe->link = kmemory_copy_string(link);
    pipe->path = kmemory_copy_string(path);
    pipe->calltime = calltime;
    pipe->callback = -1;
    pipe->kpeer = NULL;
    pipe->size = -1;
    pipe->seek = -1;
    pipe->fd = -1;
    stream_->kmap->put(stream_->kmap,kmemory_copy_string(link),KTYPE_HEAP,pipe,KTYPE_HEAP);
    stream_->kmsg->push(stream_->kmsg,kmemory_copy_string(link));
    stream_->onStart(stream,link);
    return 1;
}
Kmap* stream_sets(struct Stream *stream){
    Stream_ *stream_ = (Stream_ *) stream;
    if(stream_ == NULL){
        return NULL;
    }
    Kmap *result = kson_parse("{}");
    for(int cursor = 0 ; cursor < stream_->kmap->length(stream_->kmap) ; cursor++){
        StreamPipe *pipe = stream_->kmap->getvalue(stream_->kmap,cursor);
        if(pipe->type == STREAM_PIPE_TYPE_SET){
            result->put(result,pipe->link,KTYPE_STACK,kmemory_print_string("%li/%li",pipe->seek,pipe->size),KTYPE_HEAP);
        }
    }
    return result;
}
Kmap* stream_gets(struct Stream *stream){
    Stream_ *stream_ = (Stream_ *) stream;
    if(stream_ == NULL){
        return NULL;
    }
    Kmap *result = kson_parse("{}");
    for(int cursor = 0 ; cursor < stream_->kmap->length(stream_->kmap) ; cursor++){
        StreamPipe *pipe = stream_->kmap->getvalue(stream_->kmap,cursor);
        if(pipe->type == STREAM_PIPE_TYPE_GET){
            result->put(result,pipe->link,KTYPE_STACK,kmemory_print_string("%li/%li",pipe->seek,pipe->size),KTYPE_HEAP);
        }
    }
    return result;
}
int stream_cancel(struct Stream *stream , char *link){
    Stream_ *stream_ = (Stream_ *) stream;
    if(stream_ == NULL){
        return 0;
    }
    StreamPipe *pipe = stream_->kmap->get(stream_->kmap,link);
    if(pipe == NULL){
        return 0;
    }
    if(pipe->state == STREAM_PIPE_STATE_START){
        stream_->kmap->remove(stream_->kmap,link);
        stream_->onData(stream,link,-1);
        stream_->onStop(stream,link);
    }else{
        stream_->ktcp->close(stream_->ktcp,pipe->kpeer);
    }
    return 1;
}
StreamPipe* stream_state(struct Stream *stream , char *link){
    Stream_ *stream_ = (Stream_ *) stream;
    if(stream_ == NULL){
        return NULL;
    }
    return stream_->kmap->get(stream_->kmap,link);
}
int stream_stop(struct Stream *stream){
    Stream_ *stream_ = (Stream_ *) stream;
    if(stream_ == NULL){
        return 0;
    }
    stream_->ktcp->shutdown(stream_->ktcp);
    stream_->kprocessor->stop(stream_->kprocessor);
    if(stream_->id != NULL){
        kmemory_free(stream_->id);
    }
    if(stream_->scode != NULL){
        kmemory_free(stream_->scode);
    }
    return 1;
}
char *stream_encode(struct Stream *stream , Kmap *kmap){
    Stream_ *stream_ = (Stream_ *) stream;
    if(stream_ == NULL){
        return NULL;
    }
    char *clear = kson_pack(kmap);
    return clear;
}
Kmap *stream_decode(struct Stream *stream , char *cipher){
    Stream_ *stream_ = (Stream_ *) stream;
    if(stream_ == NULL){
        return NULL;
    }
//    return ...;
    return NULL;
}


Stream *stream_new(
        StreamParallelism parallelism,
        int buffer_size,
        int pool_size,
        char *stream_host,
        int stream_port,
        void (*onStart)(struct Stream *stream , char *link),
        void (*onOpen)(struct Stream *stream , char *link),
        void (*onHandshake)(struct Stream *stream , char *link),
        void (*onData)(struct Stream *stream , char *link , long seek),
        void (*onStop)(struct Stream *stream , char *link)
){
    Stream_ *stream_ = kmemory_alloc(sizeof(Stream_));
    stream_->interface.start = stream_start;
    stream_->interface.set = stream_set;
    stream_->interface.get = stream_get;
    stream_->interface.sets = stream_sets;
    stream_->interface.gets = stream_gets;
    stream_->interface.cancel = stream_cancel;
    stream_->interface.state = stream_state;
    stream_->interface.stop = stream_stop;
    stream_->interface.encode = stream_encode;
    stream_->interface.decode = stream_decode;
    stream_->parallelism = parallelism;
    stream_->buffer_size = buffer_size;
    stream_->stream_host = kmemory_copy_string(stream_host);
    stream_->stream_port = stream_port;
    stream_->onStart = onStart;
    stream_->onOpen = onOpen;
    stream_->onHandshake = onHandshake;
    stream_->onData = onData;
    stream_->onStop = onStop;
    stream_->id = NULL;
    stream_->scode = NULL;
    stream_->kmap = kmap_new(KCONCURRENCY_CONCURRENT,2.0f,stream_key_comparator,stream_key_destructor,stream_value_destructor);
    stream_->ktcp = ktcp_new(pool_size,buffer_size,stream_onSpawn,stream_onOpen,stream_onReadable,stream_onWritable,stream_onClose);
    stream_->kmsg = kmsg_new(KMSG_MODE_PRIVATE,sizeof(char*),10,NULL);
    stream_->ksem = ksem_new(KSEM_MODE_CREATE,NULL,1);
    stream_->kprocessor = kprocessor_new(KPROCESSOR_TYPE_THREAD,1,NULL,NULL);
    if(stream_->kmap == NULL || stream_->ktcp == NULL || stream_->kmsg == NULL || stream_->ksem == NULL || stream_->kprocessor == NULL){
        stream_free((Stream *) stream_);
        return NULL;
    }
    stream_->ktcp->bundle = stream_;
    return (Stream *) stream_;
}
void stream_free(Stream *stream){
    Stream_ *stream_ = (Stream_ *) stream;
    if(stream_ == NULL){
        return;
    }
    if(stream_->kmsg != NULL){
        stream_->kmsg->destroy(stream_->kmsg);
    }
    if(stream_->ksem != NULL){
        stream_->ksem->destroy(stream_->ksem);
    }
    kmap_free(stream_->kmap);
    ktcp_free(stream_->ktcp);
    kmsg_free(stream_->kmsg);
    ksem_free(stream_->ksem);
    kprocessor_free(stream_->kprocessor);
    kmemory_free(stream_->stream_host);
    kmemory_free(stream_);
}
