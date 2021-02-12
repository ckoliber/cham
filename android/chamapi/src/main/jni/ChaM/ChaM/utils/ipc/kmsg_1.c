#include "utils.h"

typedef struct KmsgItem{
    struct KmsgItem *next;
    void *data;
} KmsgItem;
typedef struct Kmsg_{
    struct Kmsg interface;
    KmsgMode mode;
    int msg_size;
    char *name;
#if defined(__unix__) && !defined(__ANDROID__)
    mqd_t mqd;
#endif
    Ksem *ksem_rres;
    Ksem *ksem_wres;
    Ksem *ksem_mux;
    KmsgItem *data_head;
    KmsgItem *data_tail;
} Kmsg_; // void * (kmemory_free if its CREATE if PRIV not free) -> MSG -> void * (free)

int kmsg_refresh(struct Kmsg *kmsg);
int kmsg_isempty(struct Kmsg *kmsg);
int kmsg_push(struct Kmsg *kmsg , void *data);
void* kmsg_poll(struct Kmsg *kmsg);
int kmsg_destroy(struct Kmsg *kmsg);

int kmsg_refresh(struct Kmsg *kmsg){
    Kmsg_ *kmsg_ = (Kmsg_ *) kmsg;
    if(kmsg_ == NULL){
        return 0;
    }
    if(kmsg_->mode != KMSG_MODE_PRIVATE){
#if defined(__unix__) && !defined(__ANDROID__)
        kmsg_->mqd = mq_open(kmsg_->name , O_CREAT | O_RDWR);
#endif
    }
    return 1;
}
int kmsg_isempty(struct Kmsg *kmsg){
    Kmsg_ *kmsg_ = (Kmsg_ *) kmsg;
    if(kmsg_ == NULL){
        return 0;
    }
    if(kmsg_->mode == KMSG_MODE_PRIVATE){
        return kmsg_->data_head == NULL;
    }else{
#if defined(__unix__) && !defined(__ANDROID__)
        if(kmsg_->mqd > 0){
            struct mq_attr attr;
            mq_getattr(kmsg_->mqd,&attr);
            return attr.mq_curmsgs <= 0;
        }else{
            return 0;
        }
#else
        return 0;
#endif
    }
}
int kmsg_push(struct Kmsg *kmsg , void *data){
    Kmsg_ *kmsg_ = (Kmsg_ *) kmsg;
    if(kmsg_ == NULL){
        return 0;
    }
    if(kmsg_->mode == KMSG_MODE_PRIVATE){
        kmsg_->ksem_mux->wait(kmsg_->ksem_mux);
        KmsgItem *item = kmemory_alloc(sizeof(KmsgItem));
        item->next = NULL;
        item->data = data;
        if(kmsg_->data_head == NULL){
            kmsg_->data_head = item;
        }else{
            kmsg_->data_tail->next = item;
        }
        kmsg_->data_tail = item;
        kmsg_->ksem_rres->post(kmsg_->ksem_rres);
        kmsg_->ksem_wres->wait(kmsg_->ksem_wres);
        kmsg_->ksem_mux->post(kmsg_->ksem_mux);
        return 1;
    }else{
#if defined(__unix__) && !defined(__ANDROID__)
        if(kmsg_->mqd > 0){
            return mq_send(kmsg_->mqd, data, (size_t) kmsg_->msg_size, 0) == 0;
        }else{
            return 0;
        }
#else
        return 0;
#endif
    }
} // after push ( will clone -> need free or not ))
void* kmsg_poll(struct Kmsg *kmsg){
    Kmsg_ *kmsg_ = (Kmsg_ *) kmsg;
    if(kmsg_ == NULL){
        return NULL;
    }
    void *result = NULL;
    if(kmsg_->mode == KMSG_MODE_PRIVATE){
        kmsg_->ksem_mux->wait(kmsg_->ksem_mux);
        kmsg_->ksem_rres->wait(kmsg_->ksem_rres);
        kmsg_->ksem_wres->post(kmsg_->ksem_wres);
        KmsgItem *item = kmsg_->data_head;
        kmsg_->data_head = kmsg_->data_head->next;
        kmsg_->ksem_mux->post(kmsg_->ksem_mux);
        result = item->data;
        kmemory_free(item);
    }else{
#if defined(__unix__) && !defined(__ANDROID__)
        if(kmsg_->mqd > 0){
            result = kmemory_alloc((size_t) kmsg_->msg_size);
            memset(result, 0, (size_t) kmsg_->msg_size);
            if(mq_receive(kmsg_->mqd, result, (size_t) kmsg_->msg_size, 0) < 0){
                kmemory_free(result);
                return NULL;
            }
        }
#endif
    }
    return result;
} // after poll -> free
int kmsg_destroy(struct Kmsg *kmsg){
    Kmsg_ *kmsg_ = (Kmsg_ *) kmsg;
    if(kmsg_ == NULL){
        return 0;
    }
    if(kmsg_->mode == KMSG_MODE_PRIVATE){
        while(!kmsg_isempty(kmsg)){
            kmsg_poll(kmsg);
        }
        kmsg_->ksem_rres->destroy(kmsg_->ksem_rres);
        kmsg_->ksem_wres->destroy(kmsg_->ksem_wres);
        kmsg_->ksem_mux->destroy(kmsg_->ksem_mux);
        return 1;
    }else{
#if defined(__unix__) && !defined(__ANDROID__)
        if(kmsg_->mqd > 0){
            return mq_unlink(kmsg_->name) == 0;
        }else{
            return 0;
        }
#else
        return 0;
#endif
    }
}

Kmsg *kmsg_new(KmsgMode mode , int msg_size , int msg_count , char *name){
    Kmsg_ *kmsg_ = kmemory_alloc(sizeof(Kmsg_));
    kmsg_->interface.refresh = kmsg_refresh;
    kmsg_->interface.isempty = kmsg_isempty;
    kmsg_->interface.push = kmsg_push;
    kmsg_->interface.poll = kmsg_poll;
    kmsg_->interface.destroy = kmsg_destroy;
    kmsg_->mode = mode;
    kmsg_->msg_size = msg_size;
    if(mode == KMSG_MODE_CREATE || mode == KMSG_MODE_OPEN){
#if defined(__unix__) && !defined(__ANDROID__)
        kmsg_->name = name != NULL ? kmemory_copy_string(name) : kmemory_print_token("/%li_%li");
        if(mode == KMSG_MODE_CREATE){
            mq_unlink(kmsg_->name);
        }
        struct mq_attr attr;
        attr.mq_flags = 0;
        attr.mq_maxmsg = msg_count;
        attr.mq_msgsize = kmsg_->msg_size;
        attr.mq_curmsgs = 0;
        kmsg_->mqd = mq_open(kmsg_->name,O_RDWR | O_CREAT | O_EXCL,0666,&attr);
        if(kmsg_->mqd <= 0){
            kmsg_->mqd = mq_open(kmsg_->name , 0);
        }
        kmsg_->ksem_rres = NULL;
        kmsg_->ksem_wres = NULL;
        kmsg_->ksem_mux = NULL;
        kmsg_->data_head = NULL;
        kmsg_->data_tail = NULL;

        if(kmsg_->mqd <= 0){
            kmsg_free((Kmsg *) kmsg_);
            return NULL;
        }
#else
        kmsg_free((Kmsg *) kmsg_);
        return NULL;
#endif
    }else{
        kmsg_->name = NULL;
#if defined(__unix__) && !defined(__ANDROID__)
        kmsg_->mqd = NULL;
#endif
        kmsg_->ksem_rres = ksem_new(KSEM_MODE_CREATE,NULL,0);
        kmsg_->ksem_wres = ksem_new(KSEM_MODE_CREATE,NULL,msg_count);
        kmsg_->ksem_mux = ksem_new(KSEM_MODE_CREATE,NULL,1);
        kmsg_->data_head = NULL;
        kmsg_->data_tail = NULL;

        if(kmsg_->ksem_rres == NULL || kmsg_->ksem_wres == NULL || kmsg_->ksem_mux == NULL){
            kmsg_free((Kmsg *) kmsg_);
            return NULL;
        }
    }
    return (Kmsg *) kmsg_;
}
void kmsg_free(Kmsg *kmsg){
    Kmsg_ *kmsg_ = (Kmsg_ *) kmsg;
    if(kmsg_ == NULL){
        return;
    }
    if(kmsg_->name != NULL){
        kmemory_free(kmsg_->name);
    }
#if defined(__unix__) && !defined(__ANDROID__)
    if(kmsg_->mqd > 0){
        mq_close(kmsg_->mqd);
    }
#endif
    ksem_free(kmsg_->ksem_rres);
    ksem_free(kmsg_->ksem_wres);
    ksem_free(kmsg_->ksem_mux);
    kmemory_free(kmsg_);
}