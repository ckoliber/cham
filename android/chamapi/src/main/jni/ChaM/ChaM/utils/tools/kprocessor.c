#include "utils.h"

typedef struct Kprocessor_ {
    struct Kprocessor interface;
    KprocessorType type;
    int size; // pool size
    char *name;
    void (*handler)(long token , void *result);
    struct {
        pthread_t *thread;
        pid_t *process;
    } pool;
    Kmsg *kmsg;
} Kprocessor_;
typedef struct KprocessorMessage{
    long token;
    void* (*function)(void *);
    void *arg;
} KprocessorMessage;
long kprocessor_token();
void kprocessor_looper(struct Kprocessor *kprocessor);
int kprocessor_start(struct Kprocessor *kprocessor);
long kprocessor_post(struct Kprocessor *kprocessor , void* (*function)(void*) , void *arg);
int kprocessor_stop(struct Kprocessor *kprocessor);

long kprocessor_token(){
    struct timespec time;
    clock_gettime(CLOCK_MONOTONIC,&time);
    return (time.tv_sec) * 1000000000 + (time.tv_nsec);
}
void kprocessor_looper(struct Kprocessor *kprocessor){
    Kprocessor_ *kprocessor_ = (Kprocessor_ *) kprocessor;
    if(kprocessor_ == NULL){
        return;
    }
    while(1){
        KprocessorMessage *message = kprocessor_->kmsg->poll(kprocessor_->kmsg);
        void *result = message->function(message->arg);
        if(kprocessor_->handler != NULL){
            kprocessor_->handler(message->token , result);
        }
        kmemory_free(message);
    }
}

int kprocessor_start(struct Kprocessor *kprocessor){
    Kprocessor_ *kprocessor_ = (Kprocessor_ *) kprocessor;
    if(kprocessor_ == NULL){
        return 0;
    }
    if(kprocessor_->type == KPROCESSOR_TYPE_THREAD){
        if(kprocessor_->pool.thread != NULL){
            return 0;
        }
        kprocessor_->pool.thread = kmemory_alloc(sizeof(pthread_t) * kprocessor_->size);
        for(int cursor = 0 ; cursor < kprocessor_->size ; cursor++){
            pthread_t tid;
            pthread_create(&tid , NULL , (void *(*)(void *)) kprocessor_looper , kprocessor);
            kprocessor_->pool.thread[cursor] = tid;
        }
        return 1;
    }else{
#if defined(__unix__) && !defined(__ANDROID__)
        if(kprocessor_->pool.process != NULL){
            return 0;
        }
        kprocessor_->pool.process = kmemory_alloc(sizeof(pid_t) * kprocessor_->size);
        for(int cursor = 0 ; cursor < kprocessor_->size ; cursor++){
            pid_t pid = fork();
            if(pid == 0){
                kprocessor_->kmsg->refresh(kprocessor_->kmsg);
                kprocessor_looper(kprocessor);
                break;
            }else if(pid > 0){
                kprocessor_->pool.process[cursor] = pid;
            }else{
                exit(0);
            }
        }
        return 1;
#else
        return 0;
#endif
    }
}
long kprocessor_post(struct Kprocessor *kprocessor , void* (*function)(void*) , void *arg){
    Kprocessor_ *kprocessor_ = (Kprocessor_ *) kprocessor;
    if(kprocessor_ == NULL){
        return -1;
    }
    if((kprocessor_->pool.thread == NULL && kprocessor_->pool.process == NULL) || kprocessor_->kmsg == NULL){
        return -1;
    }
    KprocessorMessage *message = kmemory_alloc(sizeof(KprocessorMessage));
    message->token = kprocessor_token();
    message->function = function;
    message->arg = arg;
    kprocessor_->kmsg->push(kprocessor_->kmsg , message);
    if(kprocessor_->type == KPROCESSOR_TYPE_PROCESS){
        kmemory_free(message);
    }
    return message->token;
}
int kprocessor_stop(struct Kprocessor *kprocessor){
    Kprocessor_ *kprocessor_ = (Kprocessor_ *) kprocessor;
    if(kprocessor_ == NULL){
        return 0;
    }
    if(kprocessor_->type == KPROCESSOR_TYPE_THREAD){
        if(kprocessor_->pool.thread == NULL || kprocessor_->kmsg == NULL){
            return 0;
        }
        for(int cursor = 0 ; cursor < kprocessor_->size ; cursor++){
            pthread_kill(kprocessor_->pool.thread[cursor],SIGKILL);
        }
        kmemory_free(kprocessor_->pool.thread);
        kprocessor_->pool.thread = NULL;
    }else{
#if defined(__unix__) && !defined(__ANDROID__)
        if(kprocessor_->pool.process == NULL || kprocessor_->kmsg == NULL){
            return 0;
        }
        for(int cursor = 0 ; cursor < kprocessor_->size ; cursor++){
            kill(kprocessor_->pool.process[cursor], SIGKILL);
        }
        kmemory_free(kprocessor_->pool.process);
        kprocessor_->pool.process = NULL;
#else
        return 0;
#endif
    }
    return 1;
}

Kprocessor *kprocessor_new(KprocessorType type , int size , char *name , void (*handler)(long token , void *result)){
    Kprocessor_ *kprocessor_ = kmemory_alloc(sizeof(Kprocessor_));
    kprocessor_->interface.start = kprocessor_start;
    kprocessor_->interface.post = kprocessor_post;
    kprocessor_->interface.stop = kprocessor_stop;
    kprocessor_->type = type;
    kprocessor_->size = size;
    kprocessor_->name = name != NULL ? kmemory_copy_string(name) : kmemory_print_token("/%li_%li");;
    kprocessor_->handler = handler;
    kprocessor_->pool.thread = NULL;
    kprocessor_->pool.process = NULL;
    if(type == KPROCESSOR_TYPE_THREAD){
        kprocessor_->kmsg = kmsg_new(KMSG_MODE_PRIVATE,sizeof(KprocessorMessage),10,name);
    }else{
        kprocessor_->kmsg = kmsg_new(KMSG_MODE_CREATE,sizeof(KprocessorMessage),10,name);
    }
    if(kprocessor_->kmsg == NULL){
        kprocessor_free((Kprocessor *) kprocessor_);
        return NULL;
    }
    return (Kprocessor *) kprocessor_;
}
void kprocessor_free(Kprocessor *kprocessor){
    Kprocessor_ *kprocessor_ = (Kprocessor_ *) kprocessor;
    if(kprocessor_ == NULL){
        return;
    }
    kprocessor_stop(kprocessor);
    if(kprocessor_->name != NULL){
        kmemory_free(kprocessor_->name);
    }
    if(kprocessor_->kmsg != NULL){
        kprocessor_->kmsg->destroy(kprocessor_->kmsg);
        kmsg_free(kprocessor_->kmsg);
    }
    kmemory_free(kprocessor_);
}