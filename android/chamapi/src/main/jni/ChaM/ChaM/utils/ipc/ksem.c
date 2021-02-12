#include "utils.h"

typedef struct Ksem_{
    struct Ksem interface;
#if !defined(__ANDROID__)
    char *name;
    sem_t *sem;
#else
    int value;
    pthread_mutex_t mutex;
    pthread_cond_t condition;
#endif
} Ksem_;

int ksem_value(struct Ksem *ksem);
int ksem_post(struct Ksem *ksem);
int ksem_wait(struct Ksem *ksem);
int ksem_destroy(struct Ksem *ksem);

int ksem_value(struct Ksem *ksem){
    Ksem_ *ksem_ = (Ksem_ *) ksem;
    if(ksem_ == NULL){
        return 0;
    }
    int value;
#if !defined(__ANDROID__)
    if(sem_getvalue(ksem_->sem , &value) < 0){
        return 0;
    }
#else
    value = ksem_->value;
#endif
    return value;
}
int ksem_post(struct Ksem *ksem){
    Ksem_ *ksem_ = (Ksem_ *) ksem;
    if(ksem_ == NULL){
        return 0;
    }
#if !defined(__ANDROID__)
    return sem_post(ksem_->sem) == 0;
#else
    pthread_mutex_lock(&ksem_->mutex);
    ksem_->value++;
    pthread_cond_broadcast(&ksem_->condition);
    pthread_mutex_unlock(&ksem_->mutex);
    return 1;
#endif
}
int ksem_wait(struct Ksem *ksem){
    Ksem_ *ksem_ = (Ksem_ *) ksem;
    if(ksem_ == NULL){
        return 0;
    }
#if !defined(__ANDROID__)
    return sem_wait(ksem_->sem) == 0;
#else
    pthread_mutex_lock(&ksem_->mutex);
    while (ksem_->value == 0) {
        pthread_cond_wait(&ksem_->condition, &ksem_->mutex);
    }
    ksem_->value--;
    pthread_mutex_unlock(&ksem_->mutex);
    return 1;
#endif
}
int ksem_destroy(struct Ksem *ksem){
    Ksem_ *ksem_ = (Ksem_ *) ksem;
    if(ksem_ == NULL){
        return 0;
    }
#if !defined(__ANDROID__)
    return sem_unlink(ksem_->name) == 0;
#else
    return 1;
#endif
}

Ksem *ksem_new(KsemMode mode , char *name , int value){
    Ksem_ *ksem_ = kmemory_alloc(sizeof(Ksem_));
    ksem_->interface.value = ksem_value;
    ksem_->interface.post = ksem_post;
    ksem_->interface.wait = ksem_wait;
    ksem_->interface.destroy = ksem_destroy;
#if !defined(__ANDROID__)
    ksem_->name = name != NULL ? kmemory_copy_string(name) : kmemory_print_token("/%li_%li");
    if(mode == KSEM_MODE_CREATE){
        sem_unlink(ksem_->name);
    }
    ksem_->sem = sem_open(ksem_->name , O_CREAT | O_EXCL , 0666 , value);
    if(ksem_->sem == NULL){
        ksem_->sem = sem_open(ksem_->name , 0);
    }
    if(ksem_->sem == NULL){
        ksem_kmemory_free((Ksem *) ksem_);
        return NULL;
    }
#else
    ksem_->value = value;
    pthread_mutex_init(&ksem_->mutex,NULL);
    pthread_cond_init(&ksem_->condition,NULL);
#endif
    return (Ksem *) ksem_;
}
void ksem_free(Ksem *ksem){
    Ksem_ *ksem_ = (Ksem_ *) ksem;
    if(ksem_ == NULL){
        return;
    }
#if !defined(__ANDROID__)
    if(ksem_->name != NULL){
        kmemory_free(ksem_->name);
    }
    if(ksem_->sem != NULL){
        sem_close(ksem_->sem);
    }
#else
    pthread_mutex_destroy(&ksem_->mutex);
    pthread_cond_destroy(&ksem_->condition);
#endif
    kmemory_free(ksem_);
}
