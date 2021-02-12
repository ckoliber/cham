#include "caster.h"

typedef struct Caster_{
    struct Caster interface;
    char *caster_host;
    int caster_port;
    void (*onTune)(struct Caster *caster , char *target_id , CasterTune tune);
    void (*onCast)(struct Caster *caster , char *target_id , void *data , int size);
    char *id;
    char *scode;
    Kmap *kmap;
    Kudp *kudp;
    Kprocessor *kprocessor;
} Caster_;
typedef struct CasterItem{
    CasterTune tune;
    KudpPeer *kpeer;
} CasterItem;

void *caster_looper(Caster *caster);
CasterTune caster_text_to_tune(char *text);
char *caster_tune_to_text(CasterTune tune);
void caster_onSpawn(struct Kudp *kudp);
void caster_onOpen(struct Kudp *kudp , KudpPeer *kpeer , void *arg);
void caster_onData(struct Kudp *kudp , KudpPeer *kpeer , char *data , int size);
void caster_onClose(struct Kudp *kudp , KudpPeer *kpeer);
int caster_key_comperator(void *key_1 , void *key_2);
void caster_key_destructor(void *key);
void caster_value_destructor(void *value);

int caster_start(struct Caster *caster , char *setting_id , char *setting_scode);
Kmap* caster_state(struct Caster *caster);
int caster_stop(struct Caster *caster);
int caster_tune(struct Caster *caster , char *target_id , CasterTune tune);
int caster_cast(struct Caster *caster , void *data , int size , int end);

void *caster_looper(Caster *caster){
    Caster_ *caster_ = (Caster_ *) caster;
    if(caster_ == NULL){
        return NULL;
    }
    while(1){
        for(int cursor = 0 ; cursor < caster_->kmap->length(caster_->kmap) ; cursor++){
            CasterItem *item = caster_->kmap->getvalue(caster_->kmap,cursor);
            if(item->kpeer != NULL){
                Kmap *response = kson_parse("{}");
                response->put(response,"DATA_TYPE",KTYPE_STACK,"PING",KTYPE_STACK);
                char *response_text = kson_pack(response);
                kmap_free(response);
                caster_->kudp->sendto(caster_->kudp,item->kpeer,response_text,(int) (strlen(response_text) + 1),0);
                kmemory_free(response_text);
            }
        }
        sleep(3);
    }
    return NULL;
}
CasterTune caster_text_to_tune(char *text){
    if(strcmp(text,"OPEN") == 0){
        return CASTER_TUNE_OPEN;
    }else if(strcmp(text,"RDWR") == 0){
        return CASTER_TUNE_RDWR;
    }else if(strcmp(text,"RDONLY") == 0){
        return CASTER_TUNE_RDONLY;
    }else if(strcmp(text,"WRONLY") == 0){
        return CASTER_TUNE_WRONLY;
    }else if(strcmp(text,"NONE") == 0){
        return CASTER_TUNE_NONE;
    }else if(strcmp(text,"WAIT") == 0){
        return CASTER_TUNE_WAIT;
    }else{
        return CASTER_TUNE_CLOSE;
    }
}
char *caster_tune_to_text(CasterTune tune){
    switch (tune){
        case CASTER_TUNE_OPEN:
            return "OPEN";
        case CASTER_TUNE_RDWR:
            return "RDWR";
        case CASTER_TUNE_RDONLY:
            return "RDONLY";
        case CASTER_TUNE_WRONLY:
            return "WRONLY";
        case CASTER_TUNE_NONE:
            return "NONE";
        case CASTER_TUNE_WAIT:
            return "WAIT";
        default:
            return "CLOSE";
    }
}
void caster_onSpawn(struct Kudp *kudp){}
void caster_onOpen(struct Kudp *kudp , KudpPeer *kpeer , void *arg){
    // send request to server
    Caster_ *caster_ = kudp->bundle;
    if(caster_ == NULL){
        return;
    }
    CasterItem *item = caster_->kmap->get(caster_->kmap,arg);
    if(item == NULL || item->kpeer != NULL){
        return;
    }
    item->kpeer = kmemory_copy_struct(kpeer,sizeof(KudpPeer));
    Kmap *request = kson_parse("{}");
    request->put(request,"DATA_TYPE",KTYPE_STACK,"TUNE",KTYPE_STACK);
    request->put(request,"TUNE",KTYPE_STACK,caster_tune_to_text(item->tune),KTYPE_STACK);
    request->put(request,"TYPE",KTYPE_STACK,"REQUEST",KTYPE_STACK);
    request->put(request,"TARGET_ID",KTYPE_STACK,arg,KTYPE_STACK);
    request->put(request,"ID",KTYPE_STACK,caster_->id,KTYPE_STACK);
    request->put(request,"SCODE",KTYPE_STACK,caster_->scode,KTYPE_STACK);
    char *request_text = kson_pack(request);
    kmap_free(request);
    kudp->sendto(kudp,kpeer,request_text,(int) strlen(request_text)+1,0);
    kmemory_free(request_text);
    if(item->tune == CASTER_TUNE_CLOSE){
        kudp->close(kudp,kpeer);
    }
}
void caster_onData(struct Kudp *kudp , KudpPeer *kpeer , char *data , int size){
    Caster_ *caster_ = kudp->bundle;
    if(caster_ == NULL){
        return;
    }
    if(data[0] == '{') {
        Kmap *request = kson_parse(data);
        if (request == NULL) {
            return;
        }
        char *data_type = request->get(request, "DATA_TYPE");
        if (data_type == NULL) {
            kmap_free(request);
            return;
        }
        if(strcmp(data_type,"TUNE") == 0){
            char *tune_text = request->get(request,"TUNE");
            char *type_text = request->get(request,"TYPE");
            char *target_id_text = request->get(request,"TARGET_ID");
            if(strcmp(type_text,"REQUEST") == 0){
                CasterTune tune = caster_text_to_tune(tune_text);
                if(tune == CASTER_TUNE_OPEN){
                    // none
                }else if(tune == CASTER_TUNE_CLOSE){
                    kudp->close(kudp,kpeer);
                }else if(tune != CASTER_TUNE_WAIT){
                    CasterItem *item = caster_->kmap->get(caster_->kmap,target_id_text);
                    if(item != NULL){
                        item->tune = tune;
                        Kmap *response = kson_parse("{}");
                        response->put(response,"DATA_TYPE",KTYPE_STACK,"TUNE",KTYPE_STACK);
                        response->put(response,"TUNE",KTYPE_STACK,tune_text,KTYPE_STACK);
                        response->put(response,"TYPE",KTYPE_STACK,"RESPONSE",KTYPE_STACK);
                        char *response_text = kson_pack(response);
                        kmap_free(response);
                        kudp->sendto(kudp,kpeer,response_text,(int) strlen(response_text)+1,0);
                        kmemory_free(response);
                        caster_->onTune((struct Caster *) caster_, target_id_text, item->tune);
                    }
                }
            }else if(strcmp(type_text,"RESPONSE") == 0){
                CasterTune tune = caster_text_to_tune(tune_text);
                if(tune == CASTER_TUNE_OPEN){
                    CasterItem *item = caster_->kmap->get(caster_->kmap,target_id_text);
                    if(item != NULL){
                        item->tune = CASTER_TUNE_NONE;
                        item->kpeer->host = kmemory_copy_string(request->get(request,"HOST"));
                        item->kpeer->port = atoi(request->get(request,"PORT"));
                        caster_->onTune((struct Caster *) caster_, target_id_text, item->tune);
                    }
                }else if(tune == CASTER_TUNE_CLOSE){
                    kudp->close(kudp,kpeer);
                }else if(tune != CASTER_TUNE_WAIT){
                    CasterItem *item = caster_->kmap->get(caster_->kmap,target_id_text);
                    if(item != NULL){
                        item->tune = tune;
                        caster_->onTune((struct Caster *) caster_, target_id_text, item->tune);
                    }
                }
            }
        }else if(strcmp(data_type,"PING") == 0){
            Kmap *response = kson_parse("{}");
            response->put(response,"DATA_TYPE",KTYPE_STACK,"PONG",KTYPE_STACK);
            char *response_text = kson_pack(response);
            kmap_free(response);
            kudp->sendto(kudp,kpeer,response_text,(int) (strlen(response_text) + 1),0);
            kmemory_free(response_text);
        }else if(strcmp(data_type,"PONG") == 0){}
        kmap_free(request);
    }else{
        for(int cursor = 0 ; cursor < caster_->kmap->length(caster_->kmap) ; cursor++){
            CasterItem *item = caster_->kmap->getvalue(caster_->kmap,cursor);
            if(item->kpeer->fd == kpeer->fd){
                if(strcmp(item->kpeer->host,kpeer->host) == 0 && item->kpeer->port == kpeer->port && item->tune && (item->tune == CASTER_TUNE_RDWR || item->tune == CASTER_TUNE_RDONLY)){
                    caster_->onCast((struct Caster *) caster_, caster_->kmap->getkey(caster_->kmap, cursor), data, size);
                }
                break;
            }
        }
    }
}
void caster_onClose(struct Kudp *kudp , KudpPeer *kpeer){
    Caster_ *caster_ = kudp->bundle;
    if(caster_ == NULL){
        return;
    }
    for(int cursor = 0 ; cursor < caster_->kmap->length(caster_->kmap) ; cursor++){
        CasterItem *item = caster_->kmap->getvalue(caster_->kmap,cursor);\
        if(item->kpeer != NULL){
            if(item->kpeer->fd == kpeer->fd && item->kpeer->index == kpeer->index){
                caster_->kmap->remove(caster_->kmap,caster_->kmap->getkey(caster_->kmap,cursor));
                caster_->onTune((struct Caster *) caster_, caster_->kmap->getkey(caster_->kmap, cursor), CASTER_TUNE_CLOSE);
                break;
            }
        }
    }
}
int caster_key_comperator(void *key_1 , void *key_2){
    return strcmp(key_1,key_2) == 0;
}
void caster_key_destructor(void *key){
    kmemory_free(key);
}
void caster_value_destructor(void *value){
    kmemory_free(value);
}

int caster_start(struct Caster *caster , char *setting_id , char *setting_scode){
    Caster_ *caster_ = (Caster_ *) caster;
    if(caster_ == NULL || setting_id == NULL || setting_scode == NULL){
        return 0;
    }
    if(caster_->id != NULL || caster_->scode != NULL){
        return 0;
    }
    caster_->id = kmemory_copy_string(setting_id);
    caster_->scode = kmemory_copy_string(setting_scode);
    caster_->kudp->binder(caster_->kudp,NULL);
    caster_->kprocessor->start(caster_->kprocessor);
    caster_->kprocessor->post(caster_->kprocessor,(void *(*)(void *)) caster_looper,caster);
    return 1;
}
Kmap* caster_state(struct Caster *caster){
    Caster_ *caster_ = (Caster_ *) caster;
    if(caster_ == NULL){
        return NULL;
    }
    Kmap *kmap = kson_parse("{}");
    for(int cursor = 0 ; cursor < caster_->kmap->length(caster_->kmap) ; cursor++){
        CasterItem *item = caster_->kmap->getvalue(caster_->kmap,cursor);
        kmap->put(caster_->kmap,kmemory_copy_string(caster_->kmap->getkey(caster_->kmap,cursor)),KTYPE_HEAP,kmemory_copy_string(caster_tune_to_text(item->tune)),KTYPE_HEAP);
    }
    return kmap;
}
int caster_stop(struct Caster *caster){
    Caster_ *caster_ = (Caster_ *) caster;
    if(caster_ == NULL){
        return 0;
    }
    caster_->kprocessor->stop(caster_->kprocessor);
    caster_->kudp->shutdown(caster_->kudp);
    if(caster_->id != NULL){
        kmemory_free(caster_->id);
    }
    if(caster_->scode != NULL){
        kmemory_free(caster_->scode);
    }
    return 1;
}
int caster_tune(struct Caster *caster , char *target_id , CasterTune tune){
    Caster_ *caster_ = (Caster_ *) caster;
    if(caster_ == NULL){
        return 0;
    }
    if(caster_->id == NULL || caster_->scode == NULL){
        return 0;
    }
    if(tune == CASTER_TUNE_OPEN){
        CasterItem *item = kmemory_alloc(sizeof(CasterItem));
        item->tune = CASTER_TUNE_OPEN;
        item->kpeer = NULL;
        caster_->kmap->put(caster_->kmap,kmemory_copy_string(target_id),KTYPE_HEAP,item,KTYPE_HEAP);
        caster_->onTune(caster,target_id,item->tune);
        if(!caster_->kudp->bind(caster_->kudp,target_id,caster_->caster_host,caster_->caster_port)){
            caster_->kmap->remove(caster_->kmap,target_id);
            caster_->onTune(caster,target_id,CASTER_TUNE_CLOSE);
        }
    }else if(tune == CASTER_TUNE_CLOSE){
        CasterItem *item = caster_->kmap->get(caster_->kmap,target_id);
        if(item != NULL){
            item->tune = CASTER_TUNE_CLOSE;
            Kmap *request = kson_parse("{}");
            request->put(request,"DATA_TYPE",KTYPE_STACK,"TUNE",KTYPE_STACK);
            request->put(request,"TUNE",KTYPE_STACK,"CLOSE",KTYPE_STACK);
            request->put(request,"TYPE",KTYPE_STACK,"REQUEST",KTYPE_STACK);
            char *request_text = kson_pack(request);
            kmap_free(request);
            caster_->kudp->sendto(caster_->kudp,item->kpeer,request_text,(int) strlen(request_text)+1,0);
            kmemory_free(request_text);
            caster_->onTune(caster,target_id,item->tune);
        }else{
            item = kmemory_alloc(sizeof(CasterItem));
            item->tune = CASTER_TUNE_CLOSE;
            item->kpeer = NULL;
            caster_->kmap->put(caster_->kmap,kmemory_copy_string(target_id),KTYPE_HEAP,item,KTYPE_HEAP);
            if(!caster_->kudp->bind(caster_->kudp,target_id,caster_->caster_host,caster_->caster_port)){
                caster_->kmap->remove(caster_->kmap,target_id);
            }
        }
    }else if(tune != CASTER_TUNE_WAIT){
        CasterItem *item = caster_->kmap->get(caster_->kmap,target_id);
        if(item == NULL || item->kpeer == NULL || (strcmp(item->kpeer->host,caster_->caster_host) == 0 && item->kpeer->port == caster_->caster_port)){
            return 0;
        }
        item->tune = CASTER_TUNE_WAIT;
        Kmap *request = kson_parse("{}");
        request->put(request,"DATA_TYPE",KTYPE_STACK,"TUNE",KTYPE_STACK);
        request->put(request,"TUNE",KTYPE_STACK,caster_tune_to_text(tune),KTYPE_STACK);
        request->put(request,"TYPE",KTYPE_STACK,"REQUEST",KTYPE_STACK);
        char *request_text = kson_pack(request);
        kmap_free(request);
        caster_->kudp->sendto(caster_->kudp,item->kpeer,request_text,(int) strlen(request_text)+1,0);
        kmemory_free(request_text);
        caster_->onTune(caster,target_id,item->tune);
    }
    return 1;
}
int caster_cast(struct Caster *caster , void *data , int size , int end){
    // send to all with tune wronly or rdwr
    Caster_ *caster_ = (Caster_ *) caster;
    if(caster_ == NULL){
        return 0;
    }
    if(caster_->id == NULL || caster_->scode == NULL){
        return 0;
    }
    for(int cursor = 0 ; cursor < caster_->kmap->length(caster_->kmap) ; cursor++){
        CasterItem *item = caster_->kmap->getvalue(caster_->kmap,cursor);
        if(item->tune == CASTER_TUNE_RDWR || item->tune == CASTER_TUNE_WRONLY){
            caster_->kudp->sendto(caster_->kudp,item->kpeer,data,size,end);
        }
    }
    return 1;
}


Caster *caster_new(
        int pool_size,
        int buffer_size,
        char *caster_host,
        int caster_port,
        void (*onTune)(struct Caster *caster , char *target_id , CasterTune tune),
        void (*onCast)(struct Caster *caster , char *target_id , void *data , int size)
){
    Caster_ *caster_ = kmemory_alloc(sizeof(Caster_));
    caster_->interface.start = caster_start;
    caster_->interface.state = caster_state;
    caster_->interface.stop = caster_stop;
    caster_->interface.tune = caster_tune;
    caster_->interface.cast = caster_cast;
    caster_->caster_host = kmemory_copy_string(caster_host);
    caster_->caster_port = caster_port;
    caster_->onTune = onTune;
    caster_->onCast = onCast;
    caster_->id = NULL;
    caster_->scode = NULL;
    caster_->kmap = kmap_new(KCONCURRENCY_CONCURRENT,2.0f,caster_key_comperator,caster_key_destructor,caster_value_destructor);
    caster_->kudp = kudp_new(pool_size,buffer_size,caster_onSpawn,caster_onOpen,caster_onData,caster_onClose);
    caster_->kprocessor = kprocessor_new(KPROCESSOR_TYPE_THREAD,1,NULL,NULL);
    if(caster_->kmap == NULL || caster_->kudp == NULL || caster_->kprocessor == NULL){
        caster_free((Caster *) caster_);
        return NULL;
    }
    caster_->kudp->bundle = caster_;
    return (Caster *) caster_;
}
void caster_free(Caster *caster){
    Caster_ *caster_ = (Caster_ *) caster;
    if(caster_ == NULL){
        return;
    }
    kmap_free(caster_->kmap);
    kudp_free(caster_->kudp);
    kprocessor_free(caster_->kprocessor);
    kmemory_free(caster_->caster_host);
    kmemory_free(caster_);
}