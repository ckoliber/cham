#include "utils.h"

typedef struct Kmap_{
    struct Kmap interface;
    Kconcurrency kconcurrency;
    float growth_factor;
    int (*key_comparator)(void *key_1,void *key_2);
    void (*key_destructor)(void *key);
    void (*value_destructor)(void *value);
    Kdata **key_address;
    Kdata **value_address;
    int data_length;
    int data_cursor;
    Ksem *ksem_res;
    Ksem *ksem_mux;
    int ksem_readers;
} Kmap_;

void* kmap_getkey(struct Kmap* kmap,int index);
void* kmap_getvalue(struct Kmap* kmap,int index);
int kmap_put(struct Kmap* kmap,void* key,Ktype key_type,void *value,Ktype value_type);
int kmap_remove(struct Kmap* kmap,void* key);
void* kmap_get(struct Kmap* kmap,void* key);
int kmap_indexof(struct Kmap* kmap,void* key);
int kmap_length(struct Kmap* kmap);

void* kmap_getkey(struct Kmap* kmap,int index){
    Kmap_ *kmap_ = (Kmap_ *) kmap;
    if(kmap_ == NULL){
        return NULL;
    }
    void *result = NULL;
    if(kmap_->kconcurrency == KCONCURRENCY_SYNCRONIZED){
        kmap_->ksem_res->wait(kmap_->ksem_res);
    }else if(kmap_->kconcurrency == KCONCURRENCY_CONCURRENT){
        kmap_->ksem_mux->wait(kmap_->ksem_mux);
        kmap_->ksem_readers++;
        if(kmap_->ksem_readers <= 1){
            kmap_->ksem_res->wait(kmap_->ksem_res);
        }
        kmap_->ksem_mux->post(kmap_->ksem_mux);
    }
    if(index >= 0 && index < kmap_length(kmap)){
        result = kmap_->key_address[index]->data;
    }
    if(kmap_->kconcurrency == KCONCURRENCY_SYNCRONIZED){
        kmap_->ksem_res->post(kmap_->ksem_res);
    }else if(kmap_->kconcurrency == KCONCURRENCY_CONCURRENT){
        kmap_->ksem_mux->wait(kmap_->ksem_mux);
        kmap_->ksem_readers--;
        if(kmap_->ksem_readers < 1){
            kmap_->ksem_res->post(kmap_->ksem_res);
        }
        kmap_->ksem_mux->post(kmap_->ksem_mux);
    }
    return result;
}
void* kmap_getvalue(struct Kmap* kmap,int index){
    Kmap_ *kmap_ = (Kmap_ *) kmap;
    if(kmap_ == NULL){
        return NULL;
    }
    void *result = NULL;
    if(kmap_->kconcurrency == KCONCURRENCY_SYNCRONIZED){
        kmap_->ksem_res->wait(kmap_->ksem_res);
    }else if(kmap_->kconcurrency == KCONCURRENCY_CONCURRENT){
        kmap_->ksem_mux->wait(kmap_->ksem_mux);
        kmap_->ksem_readers++;
        if(kmap_->ksem_readers <= 1){
            kmap_->ksem_res->wait(kmap_->ksem_res);
        }
        kmap_->ksem_mux->post(kmap_->ksem_mux);
    }
    if(index >= 0 && index < kmap_length(kmap)){
        result = kmap_->value_address[index]->data;
    }
    if(kmap_->kconcurrency == KCONCURRENCY_SYNCRONIZED){
        kmap_->ksem_res->post(kmap_->ksem_res);
    }else if(kmap_->kconcurrency == KCONCURRENCY_CONCURRENT){
        kmap_->ksem_mux->wait(kmap_->ksem_mux);
        kmap_->ksem_readers--;
        if(kmap_->ksem_readers < 1){
            kmap_->ksem_res->post(kmap_->ksem_res);
        }
        kmap_->ksem_mux->post(kmap_->ksem_mux);
    }
    return result;
}
int kmap_put(struct Kmap* kmap,void* key,Ktype key_type,void *value,Ktype value_type){
    Kmap_ *kmap_ = (Kmap_ *) kmap;
    if(kmap_ == NULL){
        return 0;
    }
    int index = kmap_indexof(kmap , key);
    if(kmap_->kconcurrency != KCONCURRENCY_PARALLEL){
        kmap_->ksem_res->wait(kmap_->ksem_res);
    }
    if(index != -1){
        Kdata *kvalue = kmap_->value_address[index];
        if(kvalue->type == KTYPE_HEAP){
            if(kmap_->value_destructor != NULL){
                kmap_->value_destructor(kvalue->data);
            }else{
                kmemory_free(kvalue->data);
            }
        }
        kvalue->data = value;
        kvalue->type = value_type;
    }else{
        if(kmap_->data_cursor >= kmap_->data_length){
            kmap_->data_length = (int) ((kmap_->data_length + 1) * kmap_->growth_factor);
            kmap_->key_address = realloc(kmap_->key_address , (kmap_->data_length)*sizeof(Kdata));
            kmap_->value_address = realloc(kmap_->value_address , (kmap_->data_length)*sizeof(Kdata));
        }
        Kdata *kkey = kmemory_alloc(sizeof(Kdata));
        kkey->data = key;
        kkey->type = key_type;
        kmap_->key_address[kmap_->data_cursor] = kkey;
        Kdata *kvalue = kmemory_alloc(sizeof(Kdata));
        kvalue->data = value;
        kvalue->type = value_type;
        kmap_->value_address[kmap_->data_cursor] = kvalue;
        kmap_->data_cursor++;
    }
    if(kmap_->kconcurrency != KCONCURRENCY_PARALLEL){
        kmap_->ksem_res->post(kmap_->ksem_res);
    }
    return 1;
}
int kmap_remove(struct Kmap* kmap,void* key){
    Kmap_ *kmap_ = (Kmap_ *) kmap;
    if(kmap_ == NULL){
        return 0;
    }
    int index = kmap_indexof(kmap , key);
    if(kmap_->kconcurrency != KCONCURRENCY_PARALLEL){
        kmap_->ksem_res->wait(kmap_->ksem_res);
    }
    if(index != -1){
        Kdata *kkey = kmap_->key_address[index];
        if(kkey->type == KTYPE_HEAP){
            if(kmap_->key_destructor != NULL){
                kmap_->key_destructor(kkey->data);
            }else{
                kmemory_free(kkey->data);
            }
        }
        kmemory_free(kkey);
        Kdata *kvalue = kmap_->value_address[index];
        if(kvalue->type == KTYPE_HEAP){
            if(kmap_->value_destructor != NULL){
                kmap_->value_destructor(kvalue->data);
            }else{
                kmemory_free(kvalue->data);
            }
        }
        kmemory_free(kvalue);
        memcpy(kmap_->key_address+index,kmap_->key_address+index+1,(kmap_->data_length-index)*sizeof(Kdata));
        memcpy(kmap_->value_address+index,kmap_->value_address+index+1,(kmap_->data_length-index)*sizeof(Kdata));
        kmap_->data_cursor--;
        if(kmap_->data_cursor < kmap_->data_length/kmap_->growth_factor){
            kmap_->data_length = (int) (kmap_->data_length/kmap_->growth_factor-1);
            kmap_->key_address = realloc(kmap_->key_address,(kmap_->data_length)*sizeof(Kdata));
            kmap_->value_address = realloc(kmap_->value_address,(kmap_->data_length)*sizeof(Kdata));
        }
    }
    if(kmap_->kconcurrency != KCONCURRENCY_PARALLEL){
        kmap_->ksem_res->post(kmap_->ksem_res);
    }
    return 1;
}
void* kmap_get(struct Kmap* kmap,void* key){
    Kmap_ *kmap_ = (Kmap_ *) kmap;
    if(kmap_ == NULL){
        return NULL;
    }
    int index = kmap_indexof(kmap , key);
    if(index != -1){
        return kmap_getvalue(kmap , index);
    }else{
        return NULL;
    }
}
int kmap_length(struct Kmap* kmap){
    Kmap_ *kmap_ = (Kmap_ *) kmap;
    if(kmap_ == NULL){
        return -1;
    }
    return kmap_->data_cursor;
}
int kmap_indexof(struct Kmap* kmap,void* key){
    Kmap_ *kmap_ = (Kmap_ *) kmap;
    if(kmap_ == NULL){
        return -1;
    }
    int result = -1;
    if(kmap_->kconcurrency == KCONCURRENCY_SYNCRONIZED){
        kmap_->ksem_res->wait(kmap_->ksem_res);
    }else if(kmap_->kconcurrency == KCONCURRENCY_CONCURRENT){
        kmap_->ksem_mux->wait(kmap_->ksem_mux);
        kmap_->ksem_readers++;
        if(kmap_->ksem_readers <= 1){
            kmap_->ksem_res->wait(kmap_->ksem_res);
        }
        kmap_->ksem_mux->post(kmap_->ksem_mux);
    }
    for(int cursor = 0 ; cursor < kmap_length(kmap) ; cursor++){
        if(kmap_->key_comparator != NULL){
            if(kmap_->key_comparator(key,kmap_->key_address[cursor]->data)){
                result = cursor;
                break;
            }
        }else{
            if(key == kmap_->key_address[cursor]->data){
                result = cursor;
                break;
            }
        }
    }
    if(kmap_->kconcurrency == KCONCURRENCY_SYNCRONIZED){
        kmap_->ksem_res->post(kmap_->ksem_res);
    }else if(kmap_->kconcurrency == KCONCURRENCY_CONCURRENT){
        kmap_->ksem_mux->wait(kmap_->ksem_mux);
        kmap_->ksem_readers--;
        if(kmap_->ksem_readers < 1){
            kmap_->ksem_res->post(kmap_->ksem_res);
        }
        kmap_->ksem_mux->post(kmap_->ksem_mux);
    }
    return result;
}

Kmap *kmap_new(Kconcurrency kconcurrency , float growth_factor , int (*key_comparator)(void *key_1,void *key_2),void (*key_destructor)(void *key),void (*value_destructor)(void *value)){
    Kmap_ *kmap_ = kmemory_alloc(sizeof(Kmap_));
    kmap_->interface.getkey = kmap_getkey;
    kmap_->interface.getvalue = kmap_getvalue;
    kmap_->interface.put = kmap_put;
    kmap_->interface.remove = kmap_remove;
    kmap_->interface.get = kmap_get;
    kmap_->interface.indexof = kmap_indexof;
    kmap_->interface.length = kmap_length;
    kmap_->kconcurrency = kconcurrency;
    kmap_->growth_factor = growth_factor;
    kmap_->key_comparator = key_comparator;
    kmap_->key_destructor = key_destructor;
    kmap_->value_destructor = value_destructor;
    kmap_->key_address = kmemory_alloc(sizeof(Kdata));
    kmap_->value_address = kmemory_alloc(sizeof(Kdata));
    kmap_->data_length = 1;
    kmap_->data_cursor = 0;
    char *name_res = kmemory_print_token("/%li_%li");
    char *name_mux = kmemory_print_token("/%li_%li");
    kmap_->ksem_res = ksem_new(KSEM_MODE_CREATE,name_res,1);
    kmap_->ksem_mux = ksem_new(KSEM_MODE_CREATE,name_mux,1);
    kmemory_free(name_res);
    kmemory_free(name_mux);
    kmap_->ksem_readers = 0;

    if(kmap_->ksem_res == NULL || kmap_->ksem_mux == NULL){
        kmap_free((Kmap *) kmap_);
        return NULL;
    }
    return (Kmap *) kmap_;
}
void kmap_free(Kmap *kmap){
    Kmap_ *kmap_ = (Kmap_ *) kmap;
    if(kmap_ == NULL){
        return;
    }
    if(kmap_->ksem_res != NULL && kmap_->ksem_mux != NULL && kmap_->key_address != NULL && kmap_->value_address != NULL){
        if(kmap_->kconcurrency != KCONCURRENCY_PARALLEL){
            kmap_->ksem_res->wait(kmap_->ksem_res);
        }
        for(int cursor = 0 ; cursor < kmap_length(kmap) ; cursor++){
            Kdata *kkey = kmap_->key_address[cursor];
            if(kkey->type == KTYPE_HEAP){
                if(kmap_->key_destructor != NULL){
                    kmap_->key_destructor(kkey->data);
                }else{
                    kmemory_free(kkey->data);
                }
            }
            kmemory_free(kkey);
            Kdata *kvalue = kmap_->value_address[cursor];
            if(kvalue->type == KTYPE_HEAP){
                if(kmap_->value_destructor != NULL){
                    kmap_->value_destructor(kvalue->data);
                }else{
                    kmemory_free(kvalue->data);
                }
            }
            kmemory_free(kvalue);
        }
        if(kmap_->kconcurrency != KCONCURRENCY_PARALLEL){
            kmap_->ksem_res->post(kmap_->ksem_res);
        }
    }
    ksem_free(kmap_->ksem_res);
    ksem_free(kmap_->ksem_mux);
    if(kmap_->key_address != NULL){
        kmemory_free(kmap_->key_address);
    }
    if(kmap_->value_address != NULL){
        kmemory_free(kmap_->value_address);
    }
    kmemory_free(kmap_);
}