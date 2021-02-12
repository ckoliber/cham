#include "utils.h"
/*  1. NEW :
 *      alloc objects
 *      if something goes wrong
 *          kx_free()
 *          return NULL;
 *      else
 *          return kx_;
 *  2. FREE :
 *      check kx_ is not null
 *      check parts is not free then free ( if was object free it )
 *  3. OPERATOR (don't need start) :
 *      check kx_ is not null
 *  4. OPERATOR (need start) :
 *      check kx_ is not null
 *      if has kprocessor or ktcp or kudp -> check started
 *
 */
typedef struct Karray_{
    struct Karray interface;
    Kconcurrency kconcurrency;
    float growth_factor;
    int (*comparator)(void *data_1,void *data_2);
    void (*destructor)(void *data);
    Kdata **data_address;
    int data_length;
    int data_cursor;
    Ksem *ksem_res;
    Ksem *ksem_mux;
    int ksem_readers;
} Karray_;

int karray_add(struct Karray *karray , void *data , Ktype data_type);
int karray_put(struct Karray *karray , int index , void *data , Ktype data_type);
int karray_remove(struct Karray *karray , int index);
void* karray_get(struct Karray *karray , int index);
int karray_indexof(struct Karray *karray , void *data);
int karray_length(struct Karray *karray);

int karray_add(struct Karray *karray , void *data , Ktype data_type){
    Karray_ *karray_ = (Karray_ *) karray;
    if(karray_ == NULL){
        return 0;
    }
    if(karray_->kconcurrency != KCONCURRENCY_PARALLEL){
        karray_->ksem_res->wait(karray_->ksem_res);
    }
    if(karray_->data_cursor >= karray_->data_length){
        karray_->data_length = (int) ((karray_->data_length + 1) * karray_->growth_factor);
        karray_->data_address = realloc(karray_->data_address,(karray_->data_length)*sizeof(Kdata));
    }
    Kdata *kdata = kmemory_alloc(sizeof(Kdata));
    kdata->data = data;
    kdata->type = data_type;
    karray_->data_address[karray_->data_cursor] = kdata;
    karray_->data_cursor++;
    if(karray_->kconcurrency != KCONCURRENCY_PARALLEL){
        karray_->ksem_res->post(karray_->ksem_res);
    }
    return 1;
}
int karray_put(struct Karray *karray , int index , void *data , Ktype data_type){
    Karray_ *karray_ = (Karray_ *) karray;
    if(karray_ == NULL){
        return 0;
    }
    if(karray_->kconcurrency != KCONCURRENCY_PARALLEL){
        karray_->ksem_res->wait(karray_->ksem_res);
    }
    if(index >= 0 && index < karray_length(karray)){
        Kdata *kdata = karray_->data_address[index];
        if(kdata->type == KTYPE_HEAP){
            if(karray_->destructor != NULL){
                karray_->destructor(kdata->data);
            }else{
                kmemory_free(kdata->data);
            }
        }
        kdata->data = data;
        kdata->type = data_type;
    }
    if(karray_->kconcurrency != KCONCURRENCY_PARALLEL){
        karray_->ksem_res->post(karray_->ksem_res);
    }
    return 1;
}
int karray_remove(struct Karray *karray , int index){
    Karray_ *karray_ = (Karray_ *) karray;
    if(karray_ == NULL){
        return 0;
    }
    if(karray_->kconcurrency != KCONCURRENCY_PARALLEL){
        karray_->ksem_res->wait(karray_->ksem_res);
    }
    if(index >= 0 && index < karray_length(karray)){
        Kdata *kdata = karray_->data_address[index];
        if(kdata->type == KTYPE_HEAP){
            if(karray_->destructor != NULL){
                karray_->destructor(kdata->data);
            }else{
                kmemory_free(kdata->data);
            }
        }
        kmemory_free(kdata);
        memcpy(karray_->data_address+index,karray_->data_address+index+1,(karray_->data_length-index)*sizeof(Kdata));
        karray_->data_cursor--;
        if(karray_->data_cursor < karray_->data_length/karray_->growth_factor){
            karray_->data_length = (int) (karray_->data_length/karray_->growth_factor-1);
            karray_->data_address = realloc(karray_->data_address,(karray_->data_length)*sizeof(Kdata));
        }
    }
    if(karray_->kconcurrency != KCONCURRENCY_PARALLEL){
        karray_->ksem_res->post(karray_->ksem_res);
    }
    return 1;
}
void* karray_get(struct Karray *karray , int index){
    Karray_ *karray_ = (Karray_ *) karray;
    if(karray_ == NULL){
        return NULL;
    }
    void *result = NULL;
    if(karray_->kconcurrency == KCONCURRENCY_SYNCRONIZED){
        karray_->ksem_res->wait(karray_->ksem_res);
    }else if(karray_->kconcurrency == KCONCURRENCY_CONCURRENT){
        karray_->ksem_mux->wait(karray_->ksem_mux);
        karray_->ksem_readers++;
        if(karray_->ksem_readers <= 1){
            karray_->ksem_res->wait(karray_->ksem_res);
        }
        karray_->ksem_mux->post(karray_->ksem_mux);
    }
    if(index >= 0 && index < karray_length(karray)){
        result = karray_->data_address[index]->data;
    }
    if(karray_->kconcurrency == KCONCURRENCY_SYNCRONIZED){
        karray_->ksem_res->post(karray_->ksem_res);
    }else if(karray_->kconcurrency == KCONCURRENCY_CONCURRENT){
        karray_->ksem_mux->wait(karray_->ksem_mux);
        karray_->ksem_readers--;
        if(karray_->ksem_readers < 1){
            karray_->ksem_res->post(karray_->ksem_res);
        }
        karray_->ksem_mux->post(karray_->ksem_mux);
    }
    return result;
}
int karray_indexof(struct Karray *karray , void *data){
    Karray_ *karray_ = (Karray_ *) karray;
    if(karray_ == NULL){
        return -1;
    }
    int result = -1;
    if(karray_->kconcurrency == KCONCURRENCY_SYNCRONIZED){
        karray_->ksem_res->wait(karray_->ksem_res);
    }else if(karray_->kconcurrency == KCONCURRENCY_CONCURRENT){
        karray_->ksem_mux->wait(karray_->ksem_mux);
        karray_->ksem_readers++;
        if(karray_->ksem_readers <= 1){
            karray_->ksem_res->wait(karray_->ksem_res);
        }
        karray_->ksem_mux->post(karray_->ksem_mux);
    }
    for(int cursor = 0 ; cursor < karray_length(karray) ; cursor++){
        if(karray_->comparator != NULL){
            if(karray_->comparator(data,karray_->data_address[cursor]->data)){
                result = cursor;
                break;
            }
        }else{
            if(data == karray_->data_address[cursor]->data){
                result = cursor;
                break;
            }
        }
    }
    if(karray_->kconcurrency == KCONCURRENCY_SYNCRONIZED){
        karray_->ksem_res->post(karray_->ksem_res);
    }else if(karray_->kconcurrency == KCONCURRENCY_CONCURRENT){
        karray_->ksem_mux->wait(karray_->ksem_mux);
        karray_->ksem_readers--;
        if(karray_->ksem_readers < 1){
            karray_->ksem_res->post(karray_->ksem_res);
        }
        karray_->ksem_mux->post(karray_->ksem_mux);
    }
    return result;
}
int karray_length(struct Karray *karray){
    Karray_ *karray_ = (Karray_ *) karray;
    if(karray_ == NULL){
        return -1;
    }
    return karray_->data_cursor;
}

Karray *karray_new(Kconcurrency kconcurrency , float growth_factor , int (*comparator)(void *data_1,void *data_2) , void (*destructor)(void *data)){
    Karray_ *karray_ = kmemory_alloc(sizeof(Karray_));
    karray_->interface.add = karray_add;
    karray_->interface.put = karray_put;
    karray_->interface.remove = karray_remove;
    karray_->interface.get = karray_get;
    karray_->interface.indexof = karray_indexof;
    karray_->interface.length = karray_length;
    karray_->kconcurrency = kconcurrency;
    karray_->growth_factor = growth_factor;
    karray_->comparator = comparator;
    karray_->destructor = destructor;
    karray_->data_address = kmemory_alloc(sizeof(Kdata));
    karray_->data_length = 1;
    karray_->data_cursor = 0;
    char *name_res = kmemory_print_token("/%li_%li");
    char *name_mux = kmemory_print_token("/%li_%li");
    karray_->ksem_res = ksem_new(KSEM_MODE_CREATE,name_res,1);
    karray_->ksem_mux = ksem_new(KSEM_MODE_CREATE,name_mux,1);
    kmemory_free(name_res);
    kmemory_free(name_mux);
    karray_->ksem_readers = 0;

    if(karray_->ksem_res == NULL || karray_->ksem_mux == NULL){
        karray_free((Karray *) karray_);
        return NULL;
    }
    return (Karray *) karray_;
}
void karray_free(Karray *karray){
    Karray_ *karray_ = (Karray_ *) karray;
    if(karray_ == NULL){
        return;
    }
    if(karray_->ksem_res != NULL && karray_->ksem_mux != NULL && karray_->data_address != NULL){
        if(karray_->kconcurrency != KCONCURRENCY_PARALLEL){
            karray_->ksem_res->wait(karray_->ksem_res);
        }
        for(int cursor = 0 ; cursor < karray_length(karray) ; cursor++){
            Kdata *kdata = karray_->data_address[cursor];
            if(kdata->type == KTYPE_HEAP){
                if(karray_->destructor != NULL){
                    karray_->destructor(kdata->data);
                }else{
                    kmemory_free(kdata->data);
                }
            }
            kmemory_free(kdata);
        }
        if(karray_->kconcurrency != KCONCURRENCY_PARALLEL){
            karray_->ksem_res->post(karray_->ksem_res);
        }
    }
    ksem_free(karray_->ksem_res);
    ksem_free(karray_->ksem_mux);
    if(karray_->data_address != NULL){
        kmemory_free(karray_->data_address);
    }
    kmemory_free(karray_);
}