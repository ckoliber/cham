#include "protocol.h"
typedef struct Protocol_{
    struct Protocol interface;
    char *protocol_host;
    int protocol_port;
    void (*onRequest)(struct Protocol *protocol , ProtocolType type , char *token , Kmap *request);
    void (*onResponse)(struct Protocol *protocol , ProtocolType type , char *token , Kmap *response);
    ProtocolState protocol_state;
    KtcpPeer *kpeer;
    Kmsg *kmsg; // send queue
    Ktcp *ktcp; // socket for send , receive
} Protocol_;

char* protocol_token(ProtocolType type);
void protocol_onSpawn(struct Ktcp *ktcp);
void protocol_onOpen(struct Ktcp *ktcp , KtcpPeer *kpeer , void *arg);
void protocol_onReadable(struct Ktcp *ktcp , KtcpPeer *kpeer , char *data , int size);
void protocol_onWritable(struct Ktcp *ktcp , KtcpPeer *kpeer);
void protocol_onClose(struct Ktcp *ktcp , KtcpPeer *kpeer);

int protocol_start(struct Protocol *protocol);
ProtocolState protocol_state(struct Protocol *protocol);
int protocol_stop(struct Protocol *protocol);
Kmap *protocol_request(struct Protocol *protocol , ProtocolType protocol_type , va_list variable_list);
Kmap *protocol_request_list(struct Protocol *protocol , ProtocolType protocol_type , ...);
int protocol_request_size(struct Protocol *protocol , ProtocolType protocol_type);

Kmap* protocol_peer_captcha(struct Protocol *protocol , char *token , char *phone , char *type);
Kmap* protocol_peer_login(struct Protocol *protocol , char *token , char *id , char *scode , char *platform , char *version);
Kmap* protocol_peer_logout(struct Protocol *protocol , char *token);
Kmap* protocol_peer_create(struct Protocol *protocol , char *token , char *phone , char *captcha);
Kmap* protocol_peer_delete(struct Protocol *protocol , char *token);
Kmap* protocol_peer_load(struct Protocol *protocol , char *token , char *id , char *type , char *mode , char *param_1 , char *param_2);
Kmap* protocol_peer_set(struct Protocol *protocol , char *token , char *id , char *key , char *option , char *value , char *operator);
Kmap* protocol_peer_id(struct Protocol *protocol , char *token , char *key , char *type);
Kmap* protocol_connection_create(struct Protocol *protocol , char *token);
Kmap* protocol_connection_delete(struct Protocol *protocol , char *token , char *id);
Kmap* protocol_connection_set(struct Protocol *protocol , char *token , char *id , char *key , char *option , char *value , char *operator);
Kmap* protocol_connection_id(struct Protocol *protocol , char *token , char *key , char *type);
Kmap* protocol_message_set(struct Protocol *protocol , char *token , char *conection_id , char *message_type , char *forward_id , char *reply_date , char *data_1 , char *data_2);
Kmap* protocol_message_handle(struct Protocol *protocol , char *token , char *conection_id , char *handle);


char* protocol_token(ProtocolType type){
    struct timespec time;
    clock_gettime(CLOCK_MONOTONIC,&time);
    if(type == PROTOCOL_PEER_CAPTCHA){
        return kmemory_print_string("%li_%li_%s",time.tv_sec,time.tv_nsec,"PEER_CAPTCHA");
    }else if(type == PROTOCOL_PEER_LOGIN){
        return kmemory_print_string("%li_%li_%s",time.tv_sec,time.tv_nsec,"PEER_LOGIN");
    }else if(type == PROTOCOL_PEER_LOGOUT){
        return kmemory_print_string("%li_%li_%s",time.tv_sec,time.tv_nsec,"PEER_LOGOUT");
    }else if(type == PROTOCOL_PEER_CREATE){
        return kmemory_print_string("%li_%li_%s",time.tv_sec,time.tv_nsec,"PEER_CREATE");
    }else if(type == PROTOCOL_PEER_DELETE){
        return kmemory_print_string("%li_%li_%s",time.tv_sec,time.tv_nsec,"PEER_DELETE");
    }else if(type == PROTOCOL_PEER_LOAD){
        return kmemory_print_string("%li_%li_%s",time.tv_sec,time.tv_nsec,"PEER_LOAD");
    }else if(type == PROTOCOL_PEER_SET){
        return kmemory_print_string("%li_%li_%s",time.tv_sec,time.tv_nsec,"PEER_SET");
    }else if(type == PROTOCOL_PEER_ID){
        return kmemory_print_string("%li_%li_%s",time.tv_sec,time.tv_nsec,"PEER_ID");
    }else if(type == PROTOCOL_CONNECTION_CREATE){
        return kmemory_print_string("%li_%li_%s",time.tv_sec,time.tv_nsec,"CONNECTION_CREATE");
    }else if(type == PROTOCOL_CONNECTION_DELETE){
        return kmemory_print_string("%li_%li_%s",time.tv_sec,time.tv_nsec,"CONNECTION_DELETE");
    }else if(type == PROTOCOL_CONNECTION_SET){
        return kmemory_print_string("%li_%li_%s",time.tv_sec,time.tv_nsec,"CONNECTION_SET");
    }else if(type == PROTOCOL_CONNECTION_ID){
        return kmemory_print_string("%li_%li_%s",time.tv_sec,time.tv_nsec,"CONNECTION_ID");
    }else if(type == PROTOCOL_MESSAGE_SET){
        return kmemory_print_string("%li_%li_%s",time.tv_sec,time.tv_nsec,"MESSAGE_SET");
    }else if(type == PROTOCOL_MESSAGE_HANDLE){
        return kmemory_print_string("%li_%li_%s",time.tv_sec,time.tv_nsec,"MESSAGE_HANDLE");
    }else{
        return kmemory_print_string("%li_%li",time.tv_sec,time.tv_nsec);
    }
}
void protocol_onSpawn(struct Ktcp *ktcp){}
void protocol_onOpen(struct Ktcp *ktcp , KtcpPeer *kpeer , void *arg){
    Protocol_ *protocol_ = ktcp->bundle;
    if(protocol_ == NULL){
        return;
    }
    protocol_->protocol_state = PROTOCOL_STATE_OFFLINE;
    protocol_->kpeer = kmemory_copy_struct(kpeer,sizeof(KtcpPeer));
}
void protocol_onReadable(struct Ktcp *ktcp , KtcpPeer *kpeer , char *data , int size){
    Protocol_ *protocol_ = ktcp->bundle;
    if(protocol_ == NULL){
        return;
    }
    if(protocol_->kmsg == NULL){
        return;
    }
    Karray *array = kson_array_parse(data);
    if(array == NULL){
        return;
    }
    for(int cursor = 0 ; cursor < array->length(array) ; cursor++){
        Kmap *response = kson_parse(array->get(array,cursor));
        if(response == NULL){
            continue;
        }
        char *token = response->get(response , "TOKEN");
        if(token == NULL){
            kmap_free(response);
            continue;
        }
        char *data_type = response->get(response , "DATA_TYPE");
        if(data_type == NULL){
            kmap_free(response);
            continue;
        }
        if(strcmp(data_type , "PEER_CAPTCHA") == 0){
            protocol_->onResponse((struct Protocol *) protocol_, PROTOCOL_PEER_CAPTCHA, token, response);
        }else if(strcmp(data_type , "PEER_LOGIN") == 0){
            if(response->indexof(response , "ERROR") < 0){
                protocol_->protocol_state = PROTOCOL_STATE_ONLINE;
            }
            protocol_->onResponse((struct Protocol *) protocol_, PROTOCOL_PEER_LOGIN, token, response);
        }else if(strcmp(data_type , "PEER_LOGOUT") == 0){
            if(response->indexof(response , "ERROR") < 0){
                protocol_->protocol_state = PROTOCOL_STATE_OFFLINE;
            }
            protocol_->onResponse((struct Protocol *) protocol_, PROTOCOL_PEER_LOGOUT, token, response);
        }else if(strcmp(data_type , "PEER_CREATE") == 0){
            protocol_->onResponse((struct Protocol *) protocol_,PROTOCOL_PEER_CREATE,token,response);
        }else if(strcmp(data_type , "PEER_DELETE") == 0){
            protocol_->onResponse((struct Protocol *) protocol_,PROTOCOL_PEER_DELETE,token,response);
        }else if(strcmp(data_type , "PEER_LOAD") == 0){
            protocol_->onResponse((struct Protocol *) protocol_,PROTOCOL_PEER_LOAD,token,response);
        }else if(strcmp(data_type , "PEER_SET") == 0){
            protocol_->onResponse((struct Protocol *) protocol_,PROTOCOL_PEER_SET,token,response);
        }else if(strcmp(data_type , "PEER_ID") == 0){
            protocol_->onResponse((struct Protocol *) protocol_,PROTOCOL_PEER_ID,token,response);
        }else if(strcmp(data_type , "CONNECTION_CREATE") == 0){
            protocol_->onResponse((struct Protocol *) protocol_,PROTOCOL_CONNECTION_CREATE,token,response);
        }else if(strcmp(data_type , "CONNECTION_DELETE") == 0){
            protocol_->onResponse((struct Protocol *) protocol_,PROTOCOL_CONNECTION_DELETE,token,response);
        }else if(strcmp(data_type , "CONNECTION_SET") == 0){
            protocol_->onResponse((struct Protocol *) protocol_,PROTOCOL_CONNECTION_SET,token,response);
        }else if(strcmp(data_type , "CONNECTION_ID") == 0){
            protocol_->onResponse((struct Protocol *) protocol_,PROTOCOL_CONNECTION_ID,token,response);
        }else if(strcmp(data_type , "MESSAGE_SET") == 0){
            protocol_->onResponse((struct Protocol *) protocol_,PROTOCOL_MESSAGE_SET,token,response);
        }else if(strcmp(data_type , "MESSAGE_HANDLE") == 0){
            protocol_->onResponse((struct Protocol *) protocol_,PROTOCOL_MESSAGE_HANDLE,token,response);
        }
        kmap_free(response);
    }
    karray_free(array);
}
void protocol_onWritable(struct Ktcp *ktcp , KtcpPeer *kpeer){
    if(kpeer != NULL){
        return;
    }
    Protocol_ *protocol_ = ktcp->bundle;
    if(protocol_ == NULL){
        return;
    }
    if(protocol_->kmsg == NULL){
        return;
    }
    int round = 10;
    Karray *array = kson_array_parse("[]");
    while(!protocol_->kmsg->isempty(protocol_->kmsg) && round > 0){
        Kmap *request = protocol_->kmsg->poll(protocol_->kmsg);
        if(request == NULL){
            continue;
        }
        char *token = request->get(request , "TOKEN");
        if(token == NULL){
            kmap_free(request);
            continue;
        }
        char *data_type = request->get(request , "DATA_TYPE");
        if(data_type == NULL){
            kmap_free(request);
            continue;
        }
        ProtocolType protocol_type;
        if(strcmp(data_type , "PEER_CAPTCHA") == 0){
            protocol_type = PROTOCOL_PEER_CAPTCHA;
        }else if(strcmp(data_type , "PEER_LOGIN") == 0){
            protocol_type = PROTOCOL_PEER_LOGIN;
        }else if(strcmp(data_type , "PEER_LOGOUT") == 0){
            protocol_type = PROTOCOL_PEER_LOGOUT;
        }else if(strcmp(data_type , "PEER_CREATE") == 0){
            protocol_type = PROTOCOL_PEER_CREATE;
        }else if(strcmp(data_type , "PEER_DELETE") == 0){
            protocol_type = PROTOCOL_PEER_DELETE;
        }else if(strcmp(data_type , "PEER_LOAD") == 0){
            protocol_type = PROTOCOL_PEER_LOAD;
        }else if(strcmp(data_type , "PEER_SET") == 0){
            protocol_type = PROTOCOL_PEER_SET;
        }else if(strcmp(data_type , "PEER_ID") == 0){
            protocol_type = PROTOCOL_PEER_ID;
        }else if(strcmp(data_type , "CONNECTION_CREATE") == 0){
            protocol_type = PROTOCOL_CONNECTION_CREATE;
        }else if(strcmp(data_type , "CONNECTION_DELETE") == 0){
            protocol_type = PROTOCOL_CONNECTION_DELETE;
        }else if(strcmp(data_type , "CONNECTION_SET") == 0){
            protocol_type = PROTOCOL_CONNECTION_SET;
        }else if(strcmp(data_type , "CONNECTION_ID") == 0){
            protocol_type = PROTOCOL_CONNECTION_ID;
        }else if(strcmp(data_type , "MESSAGE_SET") == 0){
            protocol_type = PROTOCOL_MESSAGE_SET;
        }else if(strcmp(data_type , "MESSAGE_HANDLE") == 0){
            protocol_type = PROTOCOL_MESSAGE_HANDLE;
        }else{
            kmap_free(request);
            continue;
        }
        protocol_->onRequest((struct Protocol *) protocol_, protocol_type, token, request);
        if(protocol_->protocol_state == PROTOCOL_STATE_ONLINE){
            array->add(array,kson_pack(request),KTYPE_HEAP);
            round--;
        }else{
            Kmap *response = kson_parse("{}");
            response->put(response,"DATA_TYPE",KTYPE_STACK,data_type,KTYPE_STACK);
            response->put(response,"TOKEN",KTYPE_STACK,token,KTYPE_STACK);
            response->put(response,"ERROR",KTYPE_STACK,"TIMEOUT",KTYPE_STACK);
            protocol_->onResponse((struct Protocol *) protocol_, protocol_type, token, response);
            kmap_free(response);
        }
        kmap_free(request);
    }
    if(array->length(array) > 0){
        char *data = kson_array_pack(array);
        int size = (int) strlen(data);
        protocol_->ktcp->send(protocol_->ktcp,protocol_->kpeer,data,size);
        protocol_->ktcp->send(protocol_->ktcp,protocol_->kpeer,"\n",1);
        kmemory_free(data);
    }
    karray_free(array);
}
void protocol_onClose(struct Ktcp *ktcp , KtcpPeer *kpeer){
    Protocol_ *protocol_ = ktcp->bundle;
    if(protocol_ == NULL){
        return;
    }
    protocol_->protocol_state = PROTOCOL_STATE_START;
    if(protocol_->kpeer != NULL) {
        kmemory_free(protocol_->kpeer);
        protocol_->kpeer = NULL;
        protocol_->ktcp->connect(protocol_->ktcp,NULL,protocol_->protocol_host,protocol_->protocol_port);
    }
}


int protocol_start(struct Protocol *protocol){
    Protocol_ *protocol_ = (Protocol_ *) protocol;
    if(protocol_ == NULL){
        return 0;
    }
    protocol_->protocol_state = PROTOCOL_STATE_START;
    if(protocol_->kpeer == NULL){
        return protocol_->ktcp->connect(protocol_->ktcp,NULL,protocol_->protocol_host,protocol_->protocol_port);
    }else{
        return 0;
    }
}
ProtocolState protocol_state(struct Protocol *protocol){
    Protocol_ *protocol_ = (Protocol_ *) protocol;
    if(protocol_ == NULL){
        return PROTOCOL_STATE_STOP;
    }
    return protocol_->protocol_state;
}
int protocol_stop(struct Protocol *protocol){
    Protocol_ *protocol_ = (Protocol_ *) protocol;
    if(protocol_ == NULL){
        return 0;
    }
    if(protocol_->kpeer != NULL){
        return protocol_->ktcp->close(protocol_->ktcp,protocol_->kpeer);
    }else{
        return 0;
    }
}
Kmap *protocol_request(struct Protocol *protocol , ProtocolType protocol_type , va_list variable_list){
    Kmap *request = kson_parse("{}");
    if(protocol_type == PROTOCOL_PEER_CAPTCHA) {
        char *token = va_arg(variable_list, char*);
        char *phone = va_arg(variable_list, char*);
        char *type = va_arg(variable_list, char*);
        request->put(request, "DATA_TYPE", KTYPE_STACK, "PEER_CAPTCHA", KTYPE_STACK);
        request->put(request, "TOKEN", KTYPE_STACK, token == NULL ? protocol_token(protocol_type) : kmemory_copy_string(token), KTYPE_HEAP);
        request->put(request, "PHONE", KTYPE_STACK, phone == NULL ? kmemory_copy_string("") : kmemory_copy_string(phone), KTYPE_HEAP);
        request->put(request, "TYPE", KTYPE_STACK, type == NULL ? kmemory_copy_string("") : kmemory_copy_string(type), KTYPE_HEAP);
        return request;
    }else if(protocol_type == PROTOCOL_PEER_LOGIN){
        char *token = va_arg(variable_list, char*);
        char *id = va_arg(variable_list, char*);
        char *scode = va_arg(variable_list, char*);
        char *platform = va_arg(variable_list, char*);
        char *version = va_arg(variable_list, char*);
        request->put(request, "DATA_TYPE", KTYPE_STACK, "PEER_LOGIN", KTYPE_STACK);
        request->put(request, "TOKEN", KTYPE_STACK, token == NULL ? protocol_token(protocol_type) : kmemory_copy_string(token), KTYPE_HEAP);
        request->put(request, "ID", KTYPE_STACK, id == NULL ? kmemory_copy_string("") : kmemory_copy_string(id), KTYPE_HEAP);
        request->put(request, "SCODE", KTYPE_STACK, scode == NULL ? kmemory_copy_string("") : kmemory_copy_string(scode), KTYPE_HEAP);
        request->put(request, "PLATFORM", KTYPE_STACK, platform == NULL ? kmemory_copy_string("") : kmemory_copy_string(platform), KTYPE_HEAP);
        request->put(request, "VERSION", KTYPE_STACK, version == NULL ? kmemory_copy_string("") : kmemory_copy_string(version), KTYPE_HEAP);
        return request;
    }else if(protocol_type == PROTOCOL_PEER_LOGOUT){
        char *token = va_arg(variable_list, char*);
        request->put(request, "DATA_TYPE", KTYPE_STACK, "PEER_LOGOUT", KTYPE_STACK);
        request->put(request, "TOKEN", KTYPE_STACK, token == NULL ? protocol_token(protocol_type) : kmemory_copy_string(token), KTYPE_HEAP);
        return request;
    }else if(protocol_type == PROTOCOL_PEER_CREATE){
        char *token = va_arg(variable_list, char*);
        char *phone = va_arg(variable_list, char*);
        char *captcha = va_arg(variable_list, char*);
        request->put(request, "DATA_TYPE", KTYPE_STACK, "PEER_CREATE", KTYPE_STACK);
        request->put(request, "TOKEN", KTYPE_STACK, token == NULL ? protocol_token(protocol_type) : kmemory_copy_string(token), KTYPE_HEAP);
        request->put(request, "PHONE", KTYPE_STACK, phone == NULL ? kmemory_copy_string("") : kmemory_copy_string(phone), KTYPE_HEAP);
        request->put(request, "CAPTCHA", KTYPE_STACK, captcha == NULL ? kmemory_copy_string("") : kmemory_copy_string(captcha), KTYPE_HEAP);
        return request;
    }else if(protocol_type == PROTOCOL_PEER_DELETE){
        char *token = va_arg(variable_list, char*);
        request->put(request, "DATA_TYPE", KTYPE_STACK, "PEER_DELETE", KTYPE_STACK);
        request->put(request, "TOKEN", KTYPE_STACK, token == NULL ? protocol_token(protocol_type) : kmemory_copy_string(token), KTYPE_HEAP);
        return request;
    }else if(protocol_type == PROTOCOL_PEER_LOAD){
        char *token = va_arg(variable_list, char*);
        char *id = va_arg(variable_list, char*);
        char *type = va_arg(variable_list, char*);
        char *mode = va_arg(variable_list, char*);
        char *param_1 = va_arg(variable_list, char*);
        char *param_2 = va_arg(variable_list, char*);
        request->put(request, "DATA_TYPE", KTYPE_STACK, "PEER_LOAD", KTYPE_STACK);
        request->put(request, "TOKEN", KTYPE_STACK, token == NULL ? protocol_token(protocol_type) : kmemory_copy_string(token), KTYPE_HEAP);
        request->put(request, "ID", KTYPE_STACK, id == NULL ? kmemory_copy_string("") : kmemory_copy_string(id), KTYPE_HEAP);
        request->put(request, "TYPE", KTYPE_STACK, type == NULL ? kmemory_copy_string("") : kmemory_copy_string(type), KTYPE_HEAP);
        request->put(request, "MODE", KTYPE_STACK, mode == NULL ? kmemory_copy_string("") : kmemory_copy_string(mode), KTYPE_HEAP);
        request->put(request, "PARAM_1", KTYPE_STACK, param_1 == NULL ? kmemory_copy_string("") : kmemory_copy_string(param_1), KTYPE_HEAP);
        request->put(request, "PARAM_2", KTYPE_STACK, param_2 == NULL ? kmemory_copy_string("") : kmemory_copy_string(param_2), KTYPE_HEAP);
        return request;
    }else if(protocol_type == PROTOCOL_PEER_SET){
        char *token = va_arg(variable_list, char*);
        char *id = va_arg(variable_list, char*);
        char *key = va_arg(variable_list, char*);
        char *option = va_arg(variable_list, char*);
        char *value = va_arg(variable_list, char*);
        char *operator = va_arg(variable_list, char*);
        request->put(request, "DATA_TYPE", KTYPE_STACK, "PEER_SET", KTYPE_STACK);
        request->put(request, "TOKEN", KTYPE_STACK, token == NULL ? protocol_token(protocol_type) : kmemory_copy_string(token), KTYPE_HEAP);
        request->put(request, "ID", KTYPE_STACK, id == NULL ? kmemory_copy_string("") : kmemory_copy_string(id), KTYPE_HEAP);
        request->put(request, "KEY", KTYPE_STACK, key == NULL ? kmemory_copy_string("") : kmemory_copy_string(key), KTYPE_HEAP);
        request->put(request, "OPTION", KTYPE_STACK, option == NULL ? kmemory_copy_string("") : kmemory_copy_string(option), KTYPE_HEAP);
        request->put(request, "VALUE", KTYPE_STACK, value == NULL ? kmemory_copy_string("") : kmemory_copy_string(value), KTYPE_HEAP);
        request->put(request, "OPERATOR", KTYPE_STACK, operator == NULL ? kmemory_copy_string("") : kmemory_copy_string(operator), KTYPE_HEAP);
        return request;
    }else if(protocol_type == PROTOCOL_PEER_ID){
        char *token = va_arg(variable_list, char*);
        char *key = va_arg(variable_list, char*);
        char *type = va_arg(variable_list, char*);
        request->put(request, "DATA_TYPE", KTYPE_STACK, "PEER_ID", KTYPE_STACK);
        request->put(request, "TOKEN", KTYPE_STACK, token == NULL ? protocol_token(protocol_type) : kmemory_copy_string(token), KTYPE_HEAP);
        request->put(request, "KEY", KTYPE_STACK, key == NULL ? kmemory_copy_string("") : kmemory_copy_string(key), KTYPE_HEAP);
        request->put(request, "TYPE", KTYPE_STACK, type == NULL ? kmemory_copy_string("") : kmemory_copy_string(type), KTYPE_HEAP);
        return request;
    }else if(protocol_type == PROTOCOL_CONNECTION_CREATE){
        char *token = va_arg(variable_list, char*);
        request->put(request, "DATA_TYPE", KTYPE_STACK, "CONNECTION_CREATE", KTYPE_STACK);
        request->put(request, "TOKEN", KTYPE_STACK, token == NULL ? protocol_token(protocol_type) : kmemory_copy_string(token), KTYPE_HEAP);
        return request;
    }else if(protocol_type == PROTOCOL_CONNECTION_DELETE){
        char *token = va_arg(variable_list, char*);
        char *id = va_arg(variable_list, char*);
        request->put(request, "DATA_TYPE", KTYPE_STACK, "CONNECTION_DELETE", KTYPE_STACK);
        request->put(request, "TOKEN", KTYPE_STACK, token == NULL ? protocol_token(protocol_type) : kmemory_copy_string(token), KTYPE_HEAP);
        request->put(request, "ID", KTYPE_STACK, id == NULL ? kmemory_copy_string("") : kmemory_copy_string(id), KTYPE_HEAP);
        return request;
    }else if(protocol_type == PROTOCOL_CONNECTION_SET){
        char *token = va_arg(variable_list, char*);
        char *id = va_arg(variable_list, char*);
        char *key = va_arg(variable_list, char*);
        char *option = va_arg(variable_list, char*);
        char *value = va_arg(variable_list, char*);
        char *operator = va_arg(variable_list, char*);
        request->put(request, "DATA_TYPE", KTYPE_STACK, "CONNECTION_SET", KTYPE_STACK);
        request->put(request, "TOKEN", KTYPE_STACK, token == NULL ? protocol_token(protocol_type) : kmemory_copy_string(token), KTYPE_HEAP);
        request->put(request, "ID", KTYPE_STACK, id == NULL ? kmemory_copy_string("") : kmemory_copy_string(id), KTYPE_HEAP);
        request->put(request, "KEY", KTYPE_STACK, key == NULL ? kmemory_copy_string("") : kmemory_copy_string(key), KTYPE_HEAP);
        request->put(request, "OPTION", KTYPE_STACK, option == NULL ? kmemory_copy_string("") : kmemory_copy_string(option), KTYPE_HEAP);
        request->put(request, "VALUE", KTYPE_STACK, value == NULL ? kmemory_copy_string("") : kmemory_copy_string(value), KTYPE_HEAP);
        request->put(request, "OPERATOR", KTYPE_STACK, operator == NULL ? kmemory_copy_string("") : kmemory_copy_string(operator), KTYPE_HEAP);
        return request;
    }else if(protocol_type == PROTOCOL_CONNECTION_ID){
        char *token = va_arg(variable_list, char*);
        char *key = va_arg(variable_list, char*);
        char *type = va_arg(variable_list, char*);
        request->put(request, "DATA_TYPE", KTYPE_STACK, "CONNECTION_ID", KTYPE_STACK);
        request->put(request, "TOKEN", KTYPE_STACK, token == NULL ? protocol_token(protocol_type) : kmemory_copy_string(token), KTYPE_HEAP);
        request->put(request, "KEY", KTYPE_STACK, key == NULL ? kmemory_copy_string("") : kmemory_copy_string(key), KTYPE_HEAP);
        request->put(request, "TYPE", KTYPE_STACK, type == NULL ? kmemory_copy_string("") : kmemory_copy_string(type), KTYPE_HEAP);
        return request;
    }else if(protocol_type == PROTOCOL_MESSAGE_SET){
        char *token = va_arg(variable_list, char*);
        char *conection_id = va_arg(variable_list, char*);
        char *message_type = va_arg(variable_list, char*);
        char *forward_id = va_arg(variable_list, char*);
        char *reply_date = va_arg(variable_list, char*);
        char *data_1 = va_arg(variable_list, char*);
        char *data_2 = va_arg(variable_list, char*);
        request->put(request, "DATA_TYPE", KTYPE_STACK, "MESSAGE_SET", KTYPE_STACK);
        request->put(request, "TOKEN", KTYPE_STACK, token == NULL ? protocol_token(protocol_type) : kmemory_copy_string(token), KTYPE_HEAP);
        request->put(request, "CONNECTION_ID", KTYPE_STACK, conection_id == NULL ? kmemory_copy_string("") : kmemory_copy_string(conection_id), KTYPE_HEAP);
        request->put(request, "MESSAGE_TYPE", KTYPE_STACK, message_type == NULL ? kmemory_copy_string("") : kmemory_copy_string(message_type), KTYPE_HEAP);
        request->put(request, "FORWARD_ID", KTYPE_STACK, forward_id == NULL ? kmemory_copy_string("") : kmemory_copy_string(forward_id), KTYPE_HEAP);
        request->put(request, "REPLY_DATE", KTYPE_STACK, reply_date == NULL ? kmemory_copy_string("") : kmemory_copy_string(reply_date), KTYPE_HEAP);
        request->put(request, "DATA_1", KTYPE_STACK, data_1 == NULL ? kmemory_copy_string("") : kmemory_copy_string(data_1), KTYPE_HEAP);
        request->put(request, "DATA_2", KTYPE_STACK, data_2 == NULL ? kmemory_copy_string("") : kmemory_copy_string(data_2), KTYPE_HEAP);
        return request;
    }else if(protocol_type == PROTOCOL_MESSAGE_HANDLE){
        char *token = va_arg(variable_list, char*);
        char *conection_id = va_arg(variable_list, char*);
        char *handle = va_arg(variable_list, char*);
        request->put(request, "DATA_TYPE", KTYPE_STACK, "MESSAGE_SET", KTYPE_STACK);
        request->put(request, "TOKEN", KTYPE_STACK, token == NULL ? protocol_token(protocol_type) : kmemory_copy_string(token), KTYPE_HEAP);
        request->put(request, "CONNECTION_ID", KTYPE_STACK, conection_id == NULL ? kmemory_copy_string("") : kmemory_copy_string(conection_id), KTYPE_HEAP);
        request->put(request, "HANDLE", KTYPE_STACK, handle == NULL ? kmemory_copy_string("") : kmemory_copy_string(handle), KTYPE_HEAP);
        return request;
    }else{
        kmap_free(request);
        return NULL;
    }
}
Kmap *protocol_request_list(struct Protocol *protocol , ProtocolType protocol_type , ...){
    va_list variable_list;
    va_start(variable_list,protocol_request_size(protocol,protocol_type));
    Kmap *request = protocol_request(protocol,protocol_type,variable_list);
    va_end(variable_list);
    return request;
}
int protocol_request_size(struct Protocol *protocol , ProtocolType protocol_type){
    if(protocol_type == PROTOCOL_PEER_CAPTCHA) {
        return 3;
    }else if(protocol_type == PROTOCOL_PEER_LOGIN){
        return 5;
    }else if(protocol_type == PROTOCOL_PEER_LOGOUT){
        return 1;
    }else if(protocol_type == PROTOCOL_PEER_CREATE){
        return 3;
    }else if(protocol_type == PROTOCOL_PEER_DELETE){
        return 1;
    }else if(protocol_type == PROTOCOL_PEER_LOAD){
        return 6;
    }else if(protocol_type == PROTOCOL_PEER_SET){
        return 6;
    }else if(protocol_type == PROTOCOL_PEER_ID){
        return 3;
    }else if(protocol_type == PROTOCOL_CONNECTION_CREATE){
        return 1;
    }else if(protocol_type == PROTOCOL_CONNECTION_DELETE){
        return 2;
    }else if(protocol_type == PROTOCOL_CONNECTION_SET){
        return 6;
    }else if(protocol_type == PROTOCOL_CONNECTION_ID){
        return 3;
    }else if(protocol_type == PROTOCOL_MESSAGE_SET){
        return 7;
    }else if(protocol_type == PROTOCOL_MESSAGE_HANDLE){
        return 3;
    }else{
        return 0;
    }
}

Kmap* protocol_peer_captcha(struct Protocol *protocol , char *token , char *phone , char *type){
    Protocol_ *protocol_ = (Protocol_ *) protocol;
    if(protocol_ == NULL){
        return NULL;
    }
    Kmap *request = protocol_request_list(protocol,PROTOCOL_PEER_CAPTCHA,token,phone,type);
    if(!protocol_->kmsg->push(protocol_->kmsg,request)){
        kmap_free(request);
        return NULL;
    }
    return request;
}
Kmap* protocol_peer_login(struct Protocol *protocol , char *token , char *id , char *scode , char *platform , char *version){
    Protocol_ *protocol_ = (Protocol_ *) protocol;
    if(protocol_ == NULL){
        return NULL;
    }
    Kmap *request = protocol_request_list(protocol,PROTOCOL_PEER_LOGIN,token,id,scode,platform,version);
    if(!protocol_->kmsg->push(protocol_->kmsg,request)){
        kmap_free(request);
        return NULL;
    }
    return request;
}
Kmap* protocol_peer_logout(struct Protocol *protocol , char *token){
    Protocol_ *protocol_ = (Protocol_ *) protocol;
    if(protocol_ == NULL){
        return NULL;
    }
    Kmap *request = protocol_request_list(protocol,PROTOCOL_PEER_LOGOUT,token);
    if(!protocol_->kmsg->push(protocol_->kmsg,request)){
        kmap_free(request);
        return NULL;
    }
    return request;
}
Kmap* protocol_peer_create(struct Protocol *protocol , char *token , char *phone , char *captcha){
    Protocol_ *protocol_ = (Protocol_ *) protocol;
    if(protocol_ == NULL){
        return NULL;
    }
    Kmap *request = protocol_request_list(protocol,PROTOCOL_PEER_CREATE,token,phone,captcha);
    if(!protocol_->kmsg->push(protocol_->kmsg,request)){
        kmap_free(request);
        return NULL;
    }
    return request;
}
Kmap* protocol_peer_delete(struct Protocol *protocol , char *token){
    Protocol_ *protocol_ = (Protocol_ *) protocol;
    if(protocol_ == NULL){
        return NULL;
    }
    Kmap *request = protocol_request_list(protocol,PROTOCOL_PEER_DELETE,token);
    if(!protocol_->kmsg->push(protocol_->kmsg,request)){
        kmap_free(request);
        return NULL;
    }
    return request;
}
Kmap* protocol_peer_load(struct Protocol *protocol , char *token , char *id , char *type , char *mode , char *param_1 , char *param_2){
    Protocol_ *protocol_ = (Protocol_ *) protocol;
    if(protocol_ == NULL){
        return NULL;
    }
    Kmap *request = protocol_request_list(protocol,PROTOCOL_PEER_LOAD,token,id,type,mode,param_1,param_2);
    if(!protocol_->kmsg->push(protocol_->kmsg,request)){
        kmap_free(request);
        return NULL;
    }
    return request;
}
Kmap* protocol_peer_set(struct Protocol *protocol , char *token , char *id , char *key , char *option , char *value , char *operator){
    Protocol_ *protocol_ = (Protocol_ *) protocol;
    if(protocol_ == NULL){
        return NULL;
    }
    Kmap *request = protocol_request_list(protocol,PROTOCOL_PEER_SET,token,id,key,option,value,operator);
    if(!protocol_->kmsg->push(protocol_->kmsg,request)){
        kmap_free(request);
        return NULL;
    }
    return request;
}
Kmap* protocol_peer_id(struct Protocol *protocol , char *token , char *key , char *type){
    Protocol_ *protocol_ = (Protocol_ *) protocol;
    if(protocol_ == NULL){
        return NULL;
    }
    Kmap *request = protocol_request_list(protocol,PROTOCOL_PEER_ID,token,key,type);
    if(!protocol_->kmsg->push(protocol_->kmsg,request)){
        kmap_free(request);
        return NULL;
    }
    return request;
}


Kmap* protocol_connection_create(struct Protocol *protocol , char *token){
    Protocol_ *protocol_ = (Protocol_ *) protocol;
    if(protocol_ == NULL){
        return NULL;
    }
    Kmap *request = protocol_request_list(protocol,PROTOCOL_CONNECTION_CREATE,token);
    if(!protocol_->kmsg->push(protocol_->kmsg,request)){
        kmap_free(request);
        return NULL;
    }
    return request;
}
Kmap* protocol_connection_delete(struct Protocol *protocol , char *token , char *id){
    Protocol_ *protocol_ = (Protocol_ *) protocol;
    if(protocol_ == NULL){
        return NULL;
    }
    Kmap *request = protocol_request_list(protocol,PROTOCOL_CONNECTION_DELETE,token,id);
    if(!protocol_->kmsg->push(protocol_->kmsg,request)){
        kmap_free(request);
        return NULL;
    }
    return request;
}
Kmap* protocol_connection_set(struct Protocol *protocol , char *token , char *id , char *key , char *option , char *value , char *operator){
    Protocol_ *protocol_ = (Protocol_ *) protocol;
    if(protocol_ == NULL){
        return NULL;
    }
    Kmap *request = protocol_request_list(protocol,PROTOCOL_CONNECTION_SET,token,id,key,option,value,operator);
    if(!protocol_->kmsg->push(protocol_->kmsg,request)){
        kmap_free(request);
        return NULL;
    }
    return request;
}
Kmap* protocol_connection_id(struct Protocol *protocol , char *token , char *key , char *type){
    Protocol_ *protocol_ = (Protocol_ *) protocol;
    if(protocol_ == NULL){
        return NULL;
    }
    Kmap *request = protocol_request_list(protocol,PROTOCOL_CONNECTION_ID,token,key,type);
    if(!protocol_->kmsg->push(protocol_->kmsg,request)){
        kmap_free(request);
        return NULL;
    }
    return request;
}


Kmap* protocol_message_set(struct Protocol *protocol , char *token , char *conection_id , char *message_type , char *forward_id , char *reply_date , char *data_1 , char *data_2){
    Protocol_ *protocol_ = (Protocol_ *) protocol;
    if(protocol_ == NULL){
        return NULL;
    }
    Kmap *request = protocol_request_list(protocol,PROTOCOL_MESSAGE_SET,token,conection_id,message_type,forward_id,reply_date,data_1,data_2);
    if(!protocol_->kmsg->push(protocol_->kmsg,request)){
        kmap_free(request);
        return NULL;
    }
    return request;
}
Kmap* protocol_message_handle(struct Protocol *protocol , char *token , char *conection_id , char *handle){
    Protocol_ *protocol_ = (Protocol_ *) protocol;
    if(protocol_ == NULL){
        return NULL;
    }
    Kmap *request = protocol_request_list(protocol,PROTOCOL_MESSAGE_HANDLE,token,conection_id,handle);
    if(!protocol_->kmsg->push(protocol_->kmsg,request)){
        kmap_free(request);
        return NULL;
    }
    return request;
}


Protocol *protocol_new(
        char *protocol_host,
        int protocol_port,
        void (*onRequest)(struct Protocol *protocol , ProtocolType type , char *token , Kmap *request),
        void (*onResponse)(struct Protocol *protocol , ProtocolType type , char *token , Kmap *response)
){
    Protocol_ *protocol_ = kmemory_alloc(sizeof(Protocol_));
    protocol_->interface.start = protocol_start;
    protocol_->interface.state = protocol_state;
    protocol_->interface.stop = protocol_stop;
    protocol_->interface.request = protocol_request;
    protocol_->interface.request_size = protocol_request_size;
    protocol_->interface.peer_captcha = protocol_peer_captcha;
    protocol_->interface.peer_login = protocol_peer_login;
    protocol_->interface.peer_logout = protocol_peer_logout;
    protocol_->interface.peer_create = protocol_peer_create;
    protocol_->interface.peer_delete = protocol_peer_delete;
    protocol_->interface.peer_load = protocol_peer_load;
    protocol_->interface.peer_set = protocol_peer_set;
    protocol_->interface.peer_id = protocol_peer_id;
    protocol_->interface.connection_create = protocol_connection_create;
    protocol_->interface.connection_delete = protocol_connection_delete;
    protocol_->interface.connection_set = protocol_connection_set;
    protocol_->interface.connection_id = protocol_connection_id;
    protocol_->interface.message_set = protocol_message_set;
    protocol_->interface.message_handle = protocol_message_handle;
    protocol_->protocol_host = kmemory_copy_string(protocol_host);
    protocol_->protocol_port = protocol_port;
    protocol_->onRequest = onRequest;
    protocol_->onResponse = onResponse;
    protocol_->protocol_state = PROTOCOL_STATE_STOP;
    protocol_->kpeer = NULL;
    protocol_->kmsg = kmsg_new(KMSG_MODE_PRIVATE,sizeof(Kmap),10,NULL);
    protocol_->ktcp = ktcp_new(1,16384,protocol_onSpawn,protocol_onOpen,protocol_onReadable,protocol_onWritable,protocol_onClose);
    if(protocol_->kmsg == NULL || protocol_->ktcp == NULL){
        protocol_free((Protocol *) protocol_);
        return NULL;
    }
    protocol_->ktcp->bundle = protocol_;
    protocol_->ktcp->connectivity(protocol_->ktcp,NULL);
    return (Protocol *) protocol_;
}
void protocol_free(Protocol *protocol){
    Protocol_ *protocol_ = (Protocol_ *) protocol;
    if(protocol_ == NULL){
        return;
    }
    if(protocol_->ktcp != NULL){
        protocol_->ktcp->shutdown(protocol_->ktcp);
        ktcp_free(protocol_->ktcp);
    }
    if(protocol_->kmsg != NULL){
        protocol_->kmsg->destroy(protocol_->kmsg);
        kmsg_free(protocol_->kmsg);
    }
    kmemory_free(protocol_->protocol_host);
    kmemory_free(protocol_);
}
