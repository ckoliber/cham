#include "utils.h"

typedef struct Klinked{
    Kdata *data;
    struct Klinked *next;
    struct Klinked *prev;
} Klinked;
typedef struct Klist_{
    struct Klist interface;
    Kconcurrency kconcurrency;
    int (*comparator)(void *data_1,void *data_2);
    void (*destructor)(void *data);
    Klinked *data_head;
    Klinked *data_tail;
    int data_length;
    Ksem *ksem_res;
    Ksem *ksem_mux;
    int ksem_readers;
} Klist_;

int klist_add(struct Klist *klist , void *data , Ktype data_type);
int klist_put(struct Klist *klist , int index , void *data , Ktype data_type);
int klist_remove(struct Klist *klist , int index);
void* klist_get(struct Klist *klist , int index);
int klist_indexof(struct Klist *klist , void *data);
int klist_length(struct Klist *klist);


int klist_add(struct Klist *klist , void *data , Ktype data_type){
    Klist_ *klist_ = (Klist_ *) klist;
    if(klist_ == NULL){
        return 0;
    }
    if(klist_->kconcurrency != KCONCURRENCY_PARALLEL){
        klist_->ksem_res->wait(klist_->ksem_res);
    }
    Klinked *node = kmemory_alloc(sizeof(Klinked));
    node->data = kmemory_alloc(sizeof(Kdata));
    node->data->data = data;
    node->data->type = data_type;
    node->next = NULL;
    node->prev = klist_->data_tail;
    if(klist_length(klist) > 0){
        klist_->data_tail->next = node;
    }else{
        klist_->data_head = node;
    }
    klist_->data_tail = node;
    klist_->data_length++;
    if(klist_->kconcurrency != KCONCURRENCY_PARALLEL){
        klist_->ksem_res->post(klist_->ksem_res);
    }
    return 1;
}
int klist_put(struct Klist *klist , int index , void *data , Ktype data_type){
    Klist_ *klist_ = (Klist_ *) klist;
    if(klist_ == NULL){
        return 0;
    }
    if(klist_->kconcurrency != KCONCURRENCY_PARALLEL){
        klist_->ksem_res->wait(klist_->ksem_res);
    }
    if(index >= 0 && index < klist_length(klist)/2){
        Klinked *temp = klist_->data_head;
        while(index > 0){
            temp = temp->next;
            index--;
        }
        if(temp->data->type == KTYPE_HEAP){
            if(klist_->destructor != NULL){
                klist_->destructor(temp->data->data);
            }else{
                kmemory_free(temp->data->data);
            }
        }
        temp->data->data = data;
        temp->data->type = data_type;
    }else if(index >= klist_length(klist)/2 && index < klist_length(klist)){
        index = klist_length(klist) - index;
        Klinked *temp = klist_->data_tail;
        while(index > 0){
            temp = temp->prev;
            index--;
        }
        if(temp->data->type == KTYPE_HEAP){
            if(klist_->destructor != NULL){
                klist_->destructor(temp->data->data);
            }else{
                kmemory_free(temp->data->data);
            }
        }
        temp->data->data = data;
        temp->data->type = data_type;
    }
    if(klist_->kconcurrency != KCONCURRENCY_PARALLEL){
        klist_->ksem_res->post(klist_->ksem_res);
    }
    return 1;
}
int klist_remove(struct Klist *klist , int index){
    Klist_ *klist_ = (Klist_ *) klist;
    if(klist_ == NULL){
        return 0;
    }
    if(klist_->kconcurrency != KCONCURRENCY_PARALLEL){
        klist_->ksem_res->wait(klist_->ksem_res);
    }
    if(index == 0 && klist_length(klist) == 1){
        if(klist_->data_head->data->type == KTYPE_HEAP){
            if(klist_->destructor != NULL){
                klist_->destructor(klist_->data_head->data->data);
            }else{
                kmemory_free(klist_->data_head->data->data);
            }
        }
        kmemory_free(klist_->data_head->data);
        kmemory_free(klist_->data_head);
        klist_->data_head = NULL;
        klist_->data_tail = NULL;
        klist_->data_length--;
    }else if(index >= 0 && index < klist_length(klist)){
        if(index == 0){
            klist_->data_head->next->prev = NULL;
            if(klist_->data_head->data->type == KTYPE_HEAP){
                if(klist_->destructor != NULL){
                    klist_->destructor(klist_->data_head->data->data);
                }else{
                    kmemory_free(klist_->data_head->data->data);
                }
            }
            kmemory_free(klist_->data_head->data);
            Klinked *temp = klist_->data_head->next;
            kmemory_free(klist_->data_head);
            klist_->data_head = temp;
            klist_->data_length--;
        }else if(index == klist_length(klist) - 1){
            klist_->data_tail->prev->next = NULL;
            if(klist_->data_tail->data->type == KTYPE_HEAP){
                if(klist_->destructor != NULL){
                    klist_->destructor(klist_->data_tail->data->data);
                }else{
                    kmemory_free(klist_->data_tail->data->data);
                }
            }
            kmemory_free(klist_->data_tail->data);
            Klinked *temp = klist_->data_tail->prev;
            kmemory_free(klist_->data_tail);
            klist_->data_tail = temp;
            klist_->data_length--;
        }else{
            if(index >= 0 && index < klist_length(klist)/2){
                Klinked *temp = klist_->data_head;
                while(index > 0){
                    temp = temp->next;
                    index--;
                }
                temp->prev->next = temp->next;
                temp->next->prev = temp->prev;
                if(temp->data->type == KTYPE_HEAP){
                    if(klist_->destructor != NULL){
                        klist_->destructor(temp->data->data);
                    }else{
                        kmemory_free(temp->data->data);
                    }
                }
                kmemory_free(temp->data);
                kmemory_free(temp);
                klist_->data_length--;
            }else if(index >= klist_length(klist)/2 && index < klist_length(klist)){
                index = klist_length(klist) - index;
                Klinked *temp = klist_->data_tail;
                while(index > 0){
                    temp = temp->prev;
                    index--;
                }
                temp->prev->next = temp->next;
                temp->next->prev = temp->prev;
                if(temp->data->type == KTYPE_HEAP){
                    if(klist_->destructor != NULL){
                        klist_->destructor(temp->data->data);
                    }else{
                        kmemory_free(temp->data->data);
                    }
                }
                kmemory_free(temp->data);
                kmemory_free(temp);
                klist_->data_length--;
            }
        }
    }
    if(klist_->kconcurrency != KCONCURRENCY_PARALLEL){
        klist_->ksem_res->post(klist_->ksem_res);
    }
    return 1;
}
void* klist_get(struct Klist *klist , int index){
    Klist_ *klist_ = (Klist_ *) klist;
    if(klist_ == NULL){
        return NULL;
    }
    void *result = NULL;
    if(klist_->kconcurrency == KCONCURRENCY_SYNCRONIZED){
        klist_->ksem_res->wait(klist_->ksem_res);
    }else if(klist_->kconcurrency == KCONCURRENCY_CONCURRENT){
        klist_->ksem_mux->wait(klist_->ksem_mux);
        klist_->ksem_readers++;
        if(klist_->ksem_readers <= 1){
            klist_->ksem_res->wait(klist_->ksem_res);
        }
        klist_->ksem_mux->post(klist_->ksem_mux);
    }
    if(index >= 0 && index < klist_length(klist)/2){
        Klinked *temp = klist_->data_head;
        while(index > 0){
            temp = temp->next;
            index--;
        }
        result = temp->data->data;
    }else if(index >= klist_length(klist)/2 && index < klist_length(klist)){
        index = klist_length(klist) - index - 1;
        Klinked *temp = klist_->data_tail;
        while(index > 0){
            temp = temp->prev;
            index--;
        }
        result = temp->data->data;
    }
    if(klist_->kconcurrency == KCONCURRENCY_SYNCRONIZED){
        klist_->ksem_res->post(klist_->ksem_res);
    }else if(klist_->kconcurrency == KCONCURRENCY_CONCURRENT){
        klist_->ksem_mux->wait(klist_->ksem_mux);
        klist_->ksem_readers--;
        if(klist_->ksem_readers < 1){
            klist_->ksem_res->post(klist_->ksem_res);
        }
        klist_->ksem_mux->post(klist_->ksem_mux);
    }
    return result;
}
int klist_indexof(struct Klist *klist , void *data){
    Klist_ *klist_ = (Klist_ *) klist;
    if(klist_ == NULL){
        return -1;
    }
    if(klist_->data_head == NULL){ return -1; }
    if(klist_->kconcurrency == KCONCURRENCY_SYNCRONIZED){
        klist_->ksem_res->wait(klist_->ksem_res);
    }else if(klist_->kconcurrency == KCONCURRENCY_CONCURRENT){
        klist_->ksem_mux->wait(klist_->ksem_mux);
        klist_->ksem_readers++;
        if(klist_->ksem_readers <= 1){
            klist_->ksem_res->wait(klist_->ksem_res);
        }
        klist_->ksem_mux->post(klist_->ksem_mux);
    }
    int result = 0;
    Klinked *temp = klist_->data_head;
    while(temp != NULL){
        if(klist_->comparator != NULL){
            if(klist_->comparator(data,temp->data->data)){
                break;
            }
        }else{
            if(data == temp->data->data){
                break;
            }
        }
        temp = temp->next;
        result++;
    }
    if(klist_->kconcurrency == KCONCURRENCY_SYNCRONIZED){
        klist_->ksem_res->post(klist_->ksem_res);
    }else if(klist_->kconcurrency == KCONCURRENCY_CONCURRENT){
        klist_->ksem_mux->wait(klist_->ksem_mux);
        klist_->ksem_readers--;
        if(klist_->ksem_readers < 1){
            klist_->ksem_res->post(klist_->ksem_res);
        }
        klist_->ksem_mux->post(klist_->ksem_mux);
    }
    return result >= klist_length(klist) ? -1 : result;
}
int klist_length(struct Klist *klist){
    Klist_ *klist_ = (Klist_ *) klist;
    if(klist_ == NULL){
        return -1;
    }
    return klist_->data_length;
}

Klist *klist_new(Kconcurrency kconcurrency , int (*comparator)(void *data_1,void *data_2) , void (*destructor)(void *data)){
    Klist_ *klist_ = kmemory_alloc(sizeof(Klist_));
    klist_->interface.add = klist_add;
    klist_->interface.put = klist_put;
    klist_->interface.remove = klist_remove;
    klist_->interface.get = klist_get;
    klist_->interface.indexof = klist_indexof;
    klist_->interface.length = klist_length;
    klist_->kconcurrency = kconcurrency;
    klist_->comparator = comparator;
    klist_->destructor = destructor;
    klist_->data_head = NULL;
    klist_->data_tail = NULL;
    klist_->data_length = 0;
    char *name_res = kmemory_print_token("/%li_%li");
    char *name_mux = kmemory_print_token("/%li_%li");
    klist_->ksem_res = ksem_new(KSEM_MODE_CREATE,name_res,1);
    klist_->ksem_res = ksem_new(KSEM_MODE_CREATE,name_mux,1);
    kmemory_free(name_res);
    kmemory_free(name_mux);
    klist_->ksem_readers = 0;

    if(klist_->ksem_res == NULL || klist_->ksem_mux == NULL){
        klist_free((Klist *) klist_);
        return NULL;
    }
    return (Klist *) klist_;
}
void klist_free(Klist *klist){
    Klist_ *klist_ = (Klist_ *) klist;
    if(klist_ == NULL){
        return;
    }
    if(klist_->ksem_res != NULL && klist_->ksem_mux != NULL){
        while(klist_length(klist) > 0){
            klist_remove(klist , 0);
        }
    }
    ksem_free(klist_->ksem_res);
    ksem_free(klist_->ksem_mux);
    kmemory_free(klist_);
}