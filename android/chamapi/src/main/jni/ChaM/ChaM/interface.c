#include "interface.h"

typedef struct Interface_{
    struct Interface interface;
    sqlite3 *database;
    char *interface_path;
    char *self_id;
    void (*onProtocol)(struct Interface *interface , ProtocolType type , void *data , char *token , Kmap *request , Kmap *response);
    void (*onStream)(struct Interface *interface , StreamType type , void *data , char *link , long seek);
    void (*onCaster)(struct Interface *interface , CasterType type , char *target_id , void *data , int size , CasterTune tune);
    void (*onInterface)(struct Interface *interface , InterfaceType type , Kmap *message);
    Protocol *protocol;
    Stream *stream;
    Caster *caster;
    Kmap *self; // self key-value memory cache
    Kmap *items;
} Interface_;
typedef struct InterfaceItem{
    Kmap *request;
    void *data;
} InterfaceItem;


int interface_start(struct Interface *interface);
InterfaceState interface_state(struct Interface *interface);
int interface_stop(struct Interface *interface);

Kmap* interface_self_load(struct Interface *interface);
int interface_self_init(struct Interface *interface);
char* interface_self_get(struct Interface *interface , char *key);
int interface_self_set(struct Interface *interface , char *key , char *value);
int interface_self_clear(struct Interface *interface);

int interface_handle_protocol_peer_captcha(struct Interface *interface , Kmap *request , Kmap *response);
int interface_handle_protocol_peer_login(struct Interface *interface , Kmap *request , Kmap *response);
int interface_handle_protocol_peer_logout(struct Interface *interface , Kmap *request , Kmap *response);
int interface_handle_protocol_peer_create(struct Interface *interface , Kmap *request , Kmap *response);
int interface_handle_protocol_peer_delete(struct Interface *interface , Kmap *request , Kmap *response);
int interface_handle_protocol_peer_load(struct Interface *interface , Kmap *request , Kmap *response);
int interface_handle_protocol_peer_set(struct Interface *interface , Kmap *request , Kmap *response);
int interface_handle_protocol_peer_id(struct Interface *interface , Kmap *request , Kmap *response);
int interface_handle_protocol_connection_create(struct Interface *interface , Kmap *request , Kmap *response);
int interface_handle_protocol_connection_delete(struct Interface *interface , Kmap *request , Kmap *response);
int interface_handle_protocol_connection_set(struct Interface *interface , Kmap *request , Kmap *response);
int interface_handle_protocol_connection_id(struct Interface *interface , Kmap *request , Kmap *response);
int interface_handle_protocol_message_set(struct Interface *interface , Kmap *request , Kmap *response);
int interface_handle_protocol_message_handle(struct Interface *interface , Kmap *request , Kmap *response);

Kmap* interface_protocol_request(struct Interface *interface , ProtocolType type , ...);
int interface_protocol_update(struct Interface *interface , void *data, char *token  , void (*destructor)(void *old_data));
int interface_protocol_peer_captcha(struct Interface *interface , void *data , char *token , char *phone , char *type);
int interface_protocol_peer_login(struct Interface *interface , void *data , char *token , char *id , char *scode , char *platform , char *version);
int interface_protocol_peer_logout(struct Interface *interface , void *data , char *token);
int interface_protocol_peer_create(struct Interface *interface , void *data , char *token , char *phone , char *captcha);
int interface_protocol_peer_delete(struct Interface *interface , void *data , char *token);
int interface_protocol_peer_load(struct Interface *interface , void *data , char *token , char *id , char *type , char *mode , char *param_1 , char *param_2 , int local , int try);
int interface_protocol_peer_load_local(struct Interface *interface , void *data , char *token , char *id , char *type , char *mode , char *param_1 , char *param_2 , int try);
int interface_protocol_peer_load_server(struct Interface *interface , void *data , char *token , char *id , char *type , char *mode , char *param_1 , char *param_2 , int try);
int interface_protocol_peer_set(struct Interface *interface , void *data , char *token , char *id , char *key , char *option , char *value , char *operator);
int interface_protocol_peer_id(struct Interface *interface , void *data , char *token , char *key , char *type);
int interface_protocol_connection_create(struct Interface *interface , void *data , char *token);
int interface_protocol_connection_delete(struct Interface *interface , void *data , char *token , char *id);
int interface_protocol_connection_set(struct Interface *interface , void *data , char *token , char *id , char *key , char *option , char *value , char *operator);
int interface_protocol_connection_id(struct Interface *interface , void *data , char *token , char *key , char *type);
int interface_protocol_message_set(struct Interface *interface , void *data , char *token , char *conection_id , char *message_type , char *forward_id , char *reply_date , char *data_1 , char *data_2);
int interface_protocol_message_handle(struct Interface *interface , void *data , char *token , char *conection_id , char *handle);

int interface_stream_update(struct Interface *interface , void *data, char *link  , void (*destructor)(void *old_data));
int interface_stream_set(struct Interface *interface , void *data , char *link , char *path , int calltime);
int interface_stream_get(struct Interface *interface , void *data , char *link , char *path , int calltime);
Kmap* interface_stream_sets(struct Interface *interface);
Kmap* interface_stream_gets(struct Interface *interface);
int interface_stream_cancel(struct Interface *interface , char *link);
int interface_stream_clear(struct Interface *interface);
long interface_stream_size(struct Interface *interface);
Kmap* interface_stream_state(struct Interface *interface , char *link);
char* interface_stream_getdata(struct Interface *interface , char *link);
int interface_stream_setdata(struct Interface *interface , char *link , char *state);
char* interface_stream_file(struct Interface *interface , char *link , int recreate);
char* interface_stream_temp(struct Interface *interface , int recreate);
char* interface_stream_encode(struct Interface *interface , char *name , char *size , char *mime , char *checksum);
Kmap* interface_stream_decode(struct Interface *interface , char *link);

int interface_caster_tune(struct Interface *interface , char *target_id , CasterTune tune);
int interface_caster_cast(struct Interface *interface , void *data , int size , int end);

void interface_protocol_onRequest(struct Protocol *protocol , ProtocolType type , char *token , Kmap *request);
void interface_protocol_onResponse(struct Protocol *protocol , ProtocolType type , char *token , Kmap *response);
void interface_stream_onStart(struct Stream *stream , char *link);
void interface_stream_onOpen(struct Stream *stream , char *link);
void interface_stream_onHandshake(struct Stream *stream , char *link);
void interface_stream_onData(struct Stream *stream , char *link , long seek);
void interface_stream_onStop(struct Stream *stream , char *link);
void interface_caster_onTune(struct Caster *caster , char *target_id , CasterTune tune);
void interface_caster_onCast(struct Caster *caster , char *target_id , void *data , int size);

int interface_items_key_comperator(void *key_1 , void *key_2);
void interface_items_key_destructor(void *key);
void interface_items_value_destructor(void *value);

int interface_start(struct Interface *interface){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    char *path = NULL;

    if(interface_->self->indexof(interface_->self,"SETTING_ID") < 0 || interface_->self->indexof(interface_->self,"SETTING_SCODE") < 0){
        return 0;
    }

    path = kmemory_print_string("%s/%s/%s/%s",interface_->interface_path,interface_->self_id,".Data","database.db");
    if(sqlite3_open_v2(path,&interface_->database,SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE,NULL) != SQLITE_OK){
        kmemory_free(path);
        return 0;
    }
    kmemory_free(path);

    sqlite3_exec(interface_->database,"CREATE TABLE IF NOT EXISTS PEERS(ID TEXT PRIMARY KEY,INFOS TEXT,PICTURES TEXT,PEERS TEXT,CONNECTIONS TEXT)",0,0,NULL);
    sqlite3_exec(interface_->database,"CREATE TABLE IF NOT EXISTS CONNECTIONS(ID TEXT PRIMARY KEY,INFOS TEXT,PICTURES TEXT,PEERS TEXT)",0,0,NULL);
    sqlite3_exec(interface_->database,"CREATE TABLE IF NOT EXISTS MESSAGES(`PEER_ID` TEXT,`CONNECTION_ID` TEXT,`MODE` TEXT,`MESSAGE_TYPE` TEXT,`DATE` TEXT,`FORWARD_ID` TEXT,`REPLY_DATE` TEXT,`DATA_1` TEXT,`DATA_2` TEXT,`NOT_SEEN` NUMERIC,PRIMARY KEY(`CONNECTION_ID`,`DATE`,`PEER_ID`))",0,0,NULL);
    interface_->protocol->start(interface_->protocol);
    interface_->stream->start(interface_->stream,interface_->self->get(interface_->self, "SETTING_ID"),interface_->self->get(interface_->self, "SETTING_SCODE"));
    interface_->caster->start(interface_->caster,interface_->self->get(interface_->self, "SETTING_ID"),interface_->self->get(interface_->self, "SETTING_SCODE"));
    return 1;
}
InterfaceState interface_state(struct Interface *interface){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return INTERFACE_STATE_STOP;
    }
    switch (interface_->protocol->state(interface_->protocol)){
        case PROTOCOL_STATE_STOP:
            return INTERFACE_STATE_STOP;
        case PROTOCOL_STATE_START:
            return INTERFACE_STATE_START;
        case PROTOCOL_STATE_OFFLINE:
            return INTERFACE_STATE_OFFLINE;
        default:
            return INTERFACE_STATE_ONLINE;
    }
}
int interface_stop(struct Interface *interface){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(interface_->database != NULL){
        sqlite3_close_v2(interface_->database);
    }
    interface_->protocol->stop(interface_->protocol);
    interface_->stream->stop(interface_->stream);
    interface_->caster->stop(interface_->caster);
    return 1;
}


Kmap* interface_self_load(struct Interface *interface){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return NULL;
    }
    Kmap *result = NULL;
    char *path = kmemory_print_string("%s/%s/%s",interface_->interface_path,".Data","self");
    char *data = kmemory_alloc(16384);
    FILE *file = fopen(path,"r+");
    if(file != NULL){
        memset(data,0,16384);
        fgets(data,16384,file);
        fclose(file);
        result = kson_parse(data);
        if(result == NULL){
            result = kson_parse("{}");
        }
    }else{
        result = kson_parse("{}");
    }
    kmemory_free(data);
    kmemory_free(path);
    return result;
}
int interface_self_init(struct Interface *interface){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(interface_->self == NULL){
        return 0;
    }
    char *path = NULL;

    path = kmemory_print_string("%s",interface_->interface_path);
    mkdir(path,0666);
    kmemory_free(path);

    path = kmemory_print_string("%s/%s",interface_->interface_path,".Stream");
    mkdir(path,0666);
    kmemory_free(path);

    path = kmemory_print_string("%s/%s",interface_->interface_path,".Data");
    mkdir(path,0666);
    kmemory_free(path);

    if(interface_->self->indexof(interface_->self,"UI_LANGUAGE") < 0){
        interface_self_set(interface,"UI_LANGUAGE","EN");
    }
    if(interface_->self->indexof(interface_->self,"UI_PIN") < 0){
        interface_self_set(interface,"UI_PIN","");
    }
    if(interface_->self->indexof(interface_->self,"UI_LOCK") < 0){
        interface_self_set(interface,"UI_LOCK","0");
    }
    if(interface_->self->indexof(interface_->self,"STREAM_DOWNLOAD") < 0){
        interface_self_set(interface,"STREAM_DOWNLOAD","[\"/\"]");
    }
    if(interface_->self->indexof(interface_->self,"STREAM_UPLOAD") < 0){
        interface_self_set(interface,"STREAM_UPLOAD","[\"/\"]");
    }

    if(interface_->self->indexof(interface_->self,"THEME_MAIN_BACK") < 0){
        interface_self_set(interface,"THEME_MAIN_BACK","0");
    }
    if(interface_->self->indexof(interface_->self,"THEME_MAIN_ITEM") < 0){
        interface_self_set(interface,"THEME_MAIN_ITEM","-7039745");
    }
    if(interface_->self->indexof(interface_->self,"THEME_TOOLBAR_BACK") < 0){
        interface_self_set(interface,"THEME_TOOLBAR_BACK","-7039745");
    }
    if(interface_->self->indexof(interface_->self,"THEME_TOOLBAR_ITEM") < 0){
        interface_self_set(interface,"THEME_TOOLBAR_ITEM","-12042424");
    }
    if(interface_->self->indexof(interface_->self,"THEME_BUTTON_MAIN_BACK") < 0){
        interface_self_set(interface,"THEME_BUTTON_MAIN_BACK","-7169814");
    }
    if(interface_->self->indexof(interface_->self,"THEME_BUTTON_MAIN_ITEM") < 0){
        interface_self_set(interface,"THEME_BUTTON_MAIN_ITEM","-7226369");
    }
    if(interface_->self->indexof(interface_->self,"THEME_BUTTON_SEND_BACK") < 0){
        interface_self_set(interface,"THEME_BUTTON_SEND_BACK","-16777216");
    }
    if(interface_->self->indexof(interface_->self,"THEME_BUTTON_SEND_ITEM") < 0){
        interface_self_set(interface,"THEME_BUTTON_SEND_ITEM","-16777216");
    }
    if(interface_->self->indexof(interface_->self,"THEME_CARD_MAIN_BACK") < 0){
        interface_self_set(interface,"THEME_CARD_MAIN_BACK","-12042424");
    }
    if(interface_->self->indexof(interface_->self,"THEME_CARD_MAIN_ITEM") < 0){
        interface_self_set(interface,"THEME_CARD_MAIN_ITEM","-2172205");
    }
    if(interface_->self->indexof(interface_->self,"THEME_CARD_SEND_BACK") < 0){
        interface_self_set(interface,"THEME_CARD_SEND_BACK","-1");
    }
    if(interface_->self->indexof(interface_->self,"THEME_CARD_SEND_ITEM") < 0){
        interface_self_set(interface,"THEME_CARD_SEND_ITEM","-13223619");
    }
    if(interface_->self->indexof(interface_->self,"THEME_CARD_CHATME_BACK") < 0){
        interface_self_set(interface,"THEME_CARD_CHATME_BACK","-12239796");
    }
    if(interface_->self->indexof(interface_->self,"THEME_CARD_CHATME_ITEM") < 0){
        interface_self_set(interface,"THEME_CARD_CHATME_ITEM","-3681581");
    }
    if(interface_->self->indexof(interface_->self,"THEME_CARD_CHATYOU_BACK") < 0){
        interface_self_set(interface,"THEME_CARD_CHATYOU_BACK","-4730679");
    }
    if(interface_->self->indexof(interface_->self,"THEME_CARD_CHATYOU_ITEM") < 0){
        interface_self_set(interface,"THEME_CARD_CHATYOU_ITEM","-13093054");
    }
    if(interface_->self->indexof(interface_->self,"THEME_EDITTEXT_MAIN_BACK") < 0){
        interface_self_set(interface,"THEME_EDITTEXT_MAIN_BACK","-7167745");
    }
    if(interface_->self->indexof(interface_->self,"THEME_EDITTEXT_MAIN_ITEM") < 0){
        interface_self_set(interface,"THEME_EDITTEXT_MAIN_ITEM","-13750205");
    }
    if(interface_->self->indexof(interface_->self,"THEME_EDITTEXT_SEND_BACK") < 0){
        interface_self_set(interface,"THEME_EDITTEXT_SEND_BACK","-1");
    }
    if(interface_->self->indexof(interface_->self,"THEME_EDITTEXT_SEND_ITEM") < 0){
        interface_self_set(interface,"THEME_EDITTEXT_SEND_ITEM","-16777216");
    }
    if(interface_->self->indexof(interface_->self,"THEME_EDITTEXT_TOOLBAR_BACK") < 0){
        interface_self_set(interface,"THEME_EDITTEXT_TOOLBAR_BACK","-4862514");
    }
    if(interface_->self->indexof(interface_->self,"THEME_EDITTEXT_TOOLBAR_ITEM") < 0){
        interface_self_set(interface,"THEME_EDITTEXT_TOOLBAR_ITEM","-14079181");
    }
    if(interface_->self->indexof(interface_->self,"THEME_WINDOW_NAVBAR_BACK") < 0){
        interface_self_set(interface,"THEME_WINDOW_NAVBAR_BACK","-15962958");
    }
    if(interface_->self->indexof(interface_->self,"THEME_WINDOW_STATUSBAR_BACK") < 0){
        interface_self_set(interface,"THEME_WINDOW_STATUSBAR_BACK","-5071212");
    }
    if(interface_->self->indexof(interface_->self,"THEME_WINDOW_CHAT_IMAGE") < 0){
        interface_self_set(interface,"THEME_WINDOW_CHAT_IMAGE","");
    }
    if(interface_->self->indexof(interface_->self,"THEME_WINDOW_CHAT_FONT") < 0){
        interface_self_set(interface,"THEME_WINDOW_CHAT_FONT","14");
    }
    return 1;
}
char* interface_self_get(struct Interface *interface , char *key){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return NULL;
    }
    return interface_->self->get(interface_->self,key);
}
int interface_self_set(struct Interface *interface , char *key , char *value){
    // keys -> ID , SCODE , CONNECTIONS , PERMISSIONS , ...
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    char *path = kmemory_print_string("%s/%s/%s",interface_->interface_path,".Data","self");
    FILE *file = fopen(path,"w+");
    kmemory_free(path);
    if(file == NULL){
        return 0;
    }
    interface_->self->put(interface_->self,kmemory_copy_string(key),KTYPE_HEAP,kmemory_copy_string(value),KTYPE_HEAP);
    char *data = kson_pack(interface_->self);
    if(fputs(data,file) < 0){
        interface_->self->remove(interface_->self,key);
    }
    fclose(file);
    kmemory_free(data);
    return 1;
}
int interface_self_clear(struct Interface *interface){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    char *path = kmemory_print_string("%s/%s/%s",interface_->interface_path,".Data","self");
    remove(path);
    kmemory_free(path);
    for(int cursor = 0 ; cursor < interface_->self->length(interface_->self) ; cursor++){
        interface_->self->remove(interface_->self,interface_->self->getkey(interface_->self,cursor));
    }
    return 1;
}


int interface_handle_protocol_peer_captcha(struct Interface *interface , Kmap *request , Kmap *response){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(response == NULL){
        // request
        return 1;
    }else{
        // response
        if(response->indexof(response,"ERROR") < 0){
            // response ok
        }else{
            // response no
        }
        return 0;
    }
}
int interface_handle_protocol_peer_login(struct Interface *interface , Kmap *request , Kmap *response){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(response == NULL){
        // request
//        auto_login = true;
        interface_->onInterface(interface,INTERFACE_APPHANDSHAKE,NULL);
        return 1;
    }else{
        // response
        if(response->indexof(response,"ERROR") < 0){
            // response ok
            interface_->onInterface(interface,INTERFACE_APPONLINE,NULL);
        }else{
            // response no
            interface_->onInterface(interface,INTERFACE_APPOFFLINE,NULL);
            if(strcmp(response->get(response,"ERROR"),"UPDATE") == 0){
                interface_->onInterface(interface,INTERFACE_APPUPGRADE,response);
            }
        }
        return 0;
    }
}
int interface_handle_protocol_peer_logout(struct Interface *interface , Kmap *request , Kmap *response){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(response == NULL){
        // request
        return 1;
    }else{
        // response
        if(response->indexof(response,"ERROR") < 0){
            // response ok
            interface_->onInterface(interface,INTERFACE_APPOFFLINE,NULL);
        }else{
            // response no
        }
        return 0;
    }
}
int interface_handle_protocol_peer_create(struct Interface *interface , Kmap *request , Kmap *response){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(response == NULL){
        // request
        return 1;
    }else{
        // response
        if(response->indexof(response,"ERROR") < 0){
            // response ok
            interface_self_set(interface,"SETTING_ID",response->get(response,"ID"));
            interface_self_set(interface,"SETTING_SCODE",response->get(response,"SCODE"));
            interface_self_set(interface,"SETTING_PHONE",request->get(request,"PHONE"));
            // should restart application
            interface_->onInterface(interface,INTERFACE_APPRESTART,NULL);
        }else{
            // response no
        }
        return 0;
    }
}
int interface_handle_protocol_peer_delete(struct Interface *interface , Kmap *request , Kmap *response){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(response == NULL){
        // request
        return 1;
    }else{
        // response
        if(response->indexof(response,"ERROR") < 0){
            // response ok
            interface_self_clear(interface);
            // should restart application
            interface_->onInterface(interface,INTERFACE_APPRESTART,NULL);
        }else{
            // response no
        }
        return 0;
    }
}
int interface_handle_protocol_peer_load(struct Interface *interface , Kmap *request , Kmap *response){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(response == NULL){
        // request
        return 1;
    }else{
        // response
        if(response->indexof(response,"ERROR") < 0){
            // response ok
            if(response->indexof(response,"VALUE") < 0){
                return 0;
            }
            char *type = request->get(request,"TYPE");
            char *query = NULL;
            if(strcmp(type,"PEER_INFOS") == 0){
//                {
//                    PHONE:[],
//                    LINK:[],
//                    NAME:[],
//                    BIO:[],
//                    STATE:[],
//                    PICTURE:[],
//                    PERMISSION:[]
//                }
                query = kmemory_print_string("INSERT INTO PEERS(ID) VALUES('%s')",
                                             request->get(request, "ID")
                );
                sqlite3_exec(interface_->database,query,0,0,NULL);
                kmemory_free(query);

                query = kmemory_print_string("UPDATE PEERS SET %s='%s' WHERE ID='%s'",
                                             "INFOS",
                                             response->get(response,"VALUE"),
                                             request->get(request, "ID")
                );
                sqlite3_exec(interface_->database,query,0,0,NULL);
                kmemory_free(query);
            }else if(strcmp(type,"PEER_PICTURES") == 0){
//                {
//                    DATE_1:LINK_1,
//                    DATE_2:LINK_2,
//                    DATE_3:LINK_3
//                }
                query = kmemory_print_string("INSERT INTO PEERS(ID) VALUES('%s')",
                                             request->get(request, "ID")
                );
                sqlite3_exec(interface_->database,query,0,0,NULL);
                kmemory_free(query);

                query = kmemory_print_string("UPDATE PEERS SET %s='%s' WHERE ID='%s'",
                                             "PICTURES",
                                             response->get(response,"VALUE"),
                                             request->get(request, "ID")
                );
                sqlite3_exec(interface_->database,query,0,0,NULL);
                kmemory_free(query);
            }else if(strcmp(type,"PEER_PEERS") == 0){
//                {
//                    PEER_1:PERMISSION_1,
//                    PEER_2:PERMISSION_2,
//                    PEER_3:PERMISSION_3,
//                    _:DEFAULT_PERMISSION
//                }
                query = kmemory_print_string("INSERT INTO PEERS(ID) VALUES('%s')",
                                             request->get(request, "ID")
                );
                sqlite3_exec(interface_->database,query,0,0,NULL);
                kmemory_free(query);

                query = kmemory_print_string("UPDATE PEERS SET %s='%s' WHERE ID='%s'",
                                             "PEERS",
                                             response->get(response,"VALUE"),
                                             request->get(request, "ID")
                );
                sqlite3_exec(interface_->database,query,0,0,NULL);
                kmemory_free(query);
            }else if(strcmp(type,"PEER_CONNECTIONS") == 0){
//                {
//                    CONNECTION_1:PERMISSION_1,
//                    CONNECTION_2:PERMISSION_2,
//                    CONNECTION_3:PERMISSION_3
//                }
                query = kmemory_print_string("INSERT INTO PEERS(ID) VALUES('%s')",
                                             request->get(request, "ID")
                );
                sqlite3_exec(interface_->database,query,0,0,NULL);
                kmemory_free(query);

                query = kmemory_print_string("UPDATE PEERS SET %s='%s' WHERE ID='%s'",
                                             "CONNECTIONS",
                                             response->get(response,"VALUE"),
                                             request->get(request, "ID")
                );
                sqlite3_exec(interface_->database,query,0,0,NULL);
                kmemory_free(query);
            }else if(strcmp(type,"CONNECTION_INFOS") == 0){
//                {
//                    LINK:[],
//                    NAME:[],
//                    BIO:[],
//                    PICTURE:[],
//                    PERMISSION:[]
//                }
                query = kmemory_print_string("INSERT INTO CONNECTIONS(ID) VALUES('%s')",
                                             request->get(request,"ID")
                );
                sqlite3_exec(interface_->database,query,0,0,NULL);
                kmemory_free(query);

                query = kmemory_print_string("UPDATE CONNECTIONS SET %s='%s' WHERE ID='%s'",
                                             "INFOS",
                                             response->get(response,"VALUE"),
                                             request->get(request, "ID")
                );
                sqlite3_exec(interface_->database,query,0,0,NULL);
                kmemory_free(query);
            }else if(strcmp(type,"CONNECTION_PICTURES") == 0){
//                {
//                    DATE_1:LINK_1,
//                    DATE_2:LINK_2,
//                    DATE_3:LINK_3
//                }
                query = kmemory_print_string("INSERT INTO CONNECTIONS(ID) VALUES('%s')",
                                             request->get(request,"ID")
                );
                sqlite3_exec(interface_->database,query,0,0,NULL);
                kmemory_free(query);

                query = kmemory_print_string("UPDATE CONNECTIONS SET %s='%s' WHERE ID='%s'",
                                             "PICTURES",
                                             response->get(response,"VALUE"),
                                             request->get(request, "ID")
                );
                sqlite3_exec(interface_->database,query,0,0,NULL);
                kmemory_free(query);
            }else if(strcmp(type,"CONNECTION_PEERS") == 0){
//                {
//                    PEER_1:PERMISSION_1,
//                    PEER_2:PERMISSION_2,
//                    PEER_3:PERMISSION_3,
//                    _:DEFAULT_PERMISSION
//                }
                query = kmemory_print_string("INSERT INTO CONNECTIONS(ID) VALUES('%s')",
                                             request->get(request,"ID")
                );
                sqlite3_exec(interface_->database,query,0,0,NULL);
                kmemory_free(query);

                query = kmemory_print_string("UPDATE CONNECTIONS SET %s='%s' WHERE ID='%s'",
                                             "PEERS",
                                             response->get(response,"VALUE"),
                                             request->get(request, "ID")
                );
                sqlite3_exec(interface_->database,query,0,0,NULL);
                kmemory_free(query);
            }else if(strcmp(type,"MESSAGES") == 0){
//                {
//                    PEER_ID:PEER_ID,
//                    CONNECTION_ID:CONNECTION_ID,
//                    MODE:MODE,
//                    MESSAGE_TYPE:MESSAGE_TYPE,
//                    DATE:DATE,
//                    FORWARD_ID:FORWARD_ID,
//                    REPLY_DATE:REPLY_DATE,
//                    DATA_1:DATA_1,
//                    DATA_2:DATA_2
//                }
                Kmap *value = kson_parse(response->get(response,"VALUE"));
                query = kmemory_print_string("DELETE FROM MESSAGES WHERE `CONNECTION_ID`='%s' AND `DATE`='%s' AND PEER_ID='%s'",
                                             value->get(value,"CONNECTION_ID"),
                                             value->get(value,"DATE"),
                                             value->get(value,"PEER_ID")
                );
                sqlite3_exec(interface_->database,query,0,0,NULL);
                kmemory_free(query);
                query = kmemory_print_string("INSERT INTO MESSAGES(`PEER_ID`,`CONNECTION_ID`,`MODE`,`MESSAGE_TYPE`,`DATE`,`FORWARD_ID`,`REPLY_DATE`,`DATA_1`,`DATA_2`,`NOT_SEEN`) VALUES('%s','%s','%s','%s','%s','%s','%s','%s','%s','%s')",
                                             value->get(value, "PEER_ID"),
                                             value->get(value, "CONNECTION_ID"),
                                             value->get(value, "MODE"),
                                             value->get(value, "MESSAGE_TYPE"),
                                             value->get(value, "DATE"),
                                             value->get(value, "FORWARD_ID"),
                                             value->get(value, "REPLY_DATE"),
                                             value->get(value, "DATA_1"),
                                             value->get(value, "DATA_2"),
                                             "0"
                );
                sqlite3_exec(interface_->database,query,0,0,NULL);
                kmemory_free(query);
                kmap_free(value);
            }
            return 1;
        }else{
            // response no
        }
        return 0;
    }
}
int interface_handle_protocol_peer_set(struct Interface *interface , Kmap *request , Kmap *response){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(response == NULL){
        // request
        return 1;
    }else{
        // response
        if(response->indexof(response,"ERROR") < 0){
            // response ok
        }else{
            // response no
        }
        return 0;
    }
}
int interface_handle_protocol_peer_id(struct Interface *interface , Kmap *request , Kmap *response){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(response == NULL){
        // request
        return 1;
    }else{
        // response
        if(response->indexof(response,"ERROR") < 0){
            // response ok
        }else{
            // response no
        }
        return 0;
    }
}


int interface_handle_protocol_connection_create(struct Interface *interface , Kmap *request , Kmap *response){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(response == NULL){
        // request
        return 1;
    }else{
        // response
        if(response->indexof(response,"ERROR") < 0){
            // response ok
        }else{
            // response no
        }
        return 0;
    }
}
int interface_handle_protocol_connection_delete(struct Interface *interface , Kmap *request , Kmap *response){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(response == NULL){
        // request
        return 1;
    }else{
        // response
        if(response->indexof(response,"ERROR") < 0){
            // response ok
        }else{
            // response no
        }
        return 0;
    }
}
int interface_handle_protocol_connection_set(struct Interface *interface , Kmap *request , Kmap *response){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(response == NULL){
        // request
        return 1;
    }else{
        // response
        if(response->indexof(response,"ERROR") < 0){
            // response ok
        }else{
            // response no
        }
        return 0;
    }
}
int interface_handle_protocol_connection_id(struct Interface *interface , Kmap *request , Kmap *response){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(response == NULL){
        // request
        return 1;
    }else{
        // response
        if(response->indexof(response,"ERROR") < 0){
            // response ok
        }else{
            // response no
        }
        return 0;
    }
}


int interface_handle_protocol_message_set(struct Interface *interface , Kmap *request , Kmap *response){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    char *query = NULL;
    if(response == NULL){
        // request
        query = kmemory_print_string("DELETE FROM MESSAGES WHERE `CONNECTION_ID`='%s' AND `DATE`='%s'",
                                     request->get(request,"CONNECTION_ID"),
                                     request->get(request,"TOKEN")
        );
        sqlite3_exec(interface_->database,query,0,0,NULL);
        kmemory_free(query);

        query = kmemory_print_string("INSERT INTO MESSAGES(`PEER_ID`,`CONNECTION_ID`,`MODE`,`MESSAGE_TYPE`,`DATE`,`FORWARD_ID`,`REPLY_DATE`,`DATA_1`,`DATA_2`,`NOT_SEEN`) VALUES('%s','%s','%s','%s','%s','%s','%s','%s','%s','%s')",
                                     interface_self_get(interface,"SETTING_ID"),
                                     request->get(request,"CONNECTION_ID"),
                                     "0",
                                     request->get(request,"MESSAGE_TYPE"),
                                     request->get(request,"TOKEN"),
                                     request->get(request,"FORWARD_ID"),
                                     request->get(request,"REPLY_DATE"),
                                     request->get(request,"DATA_1"),
                                     request->get(request,"DATA_2"),
                                     "1"
        );
        sqlite3_exec(interface_->database,query,0,0,NULL);
        kmemory_free(query);
        return 1;
    }else{
        // response
        if(response->indexof(response,"ERROR") < 0){
            // response ok
            query = kmemory_print_string("DELETE FROM MESSAGES WHERE `CONNECTION_ID`='%s' AND `DATE`='%s'",
                                         request->get(request,"CONNECTION_ID"),
                                         response->get(response,"TOKEN")
            );
            sqlite3_exec(interface_->database,query,0,0,NULL);
            kmemory_free(query);

            if(strcmp(response->get(response,"MESSAGE_TYPE"),"EVENT") == 0){
                if(strcmp(response->get(response,"DATA_1"),"MESSAGE_DELETE") == 0){
                    // DATA_2 -> [ID , DATE]
                    Karray *karray = kson_array_parse(response->get(response,"DATA_2"));
                    query = kmemory_print_string("DELETE FROM MESSAGES WHERE `CONNECTION_ID`='%s' AND `DATE`='%s' AND `PEER_ID`='%s'",
                                                 response->get(response,"CONNECTION_ID"),
                                                 karray->get(karray,1),
                                                 karray->get(karray,0)
                    );
                    sqlite3_exec(interface_->database,query,0,0,NULL);
                    kmemory_free(query);
                    karray_free(karray);
                }else if(strcmp(response->get(response,"DATA_1"),"MESSAGE_EDIT") == 0){
                    // DATA_2 -> [DATE , DATA_1 , DATA_2] , REPLY_DATE
                    Karray *karray = kson_array_parse(response->get(response,"DATA_2"));
                    query = kmemory_print_string("UPDATE MESSAGES SET `DATA_1`='%s' , `DATA_2`='%s' , `REPLY_DATE`='%s' WHERE `CONNECTION_ID`='%s' AND `DATE`='%s' AND `PEER_ID`='%s'",
                                                 karray->get(karray,1),
                                                 karray->get(karray,2),
                                                 response->get(response,"REPLY_DATE"),
                                                 response->get(response,"CONNECTION_ID"),
                                                 karray->get(karray,0),
                                                 response->get(response,"PEER_ID")
                    );
                    sqlite3_exec(interface_->database,query,0,0,NULL);
                    kmemory_free(query);
                    karray_free(karray);
                }else if(strcmp(response->get(response,"DATA_1"),"MESSAGE_READ") == 0){
                    // DATA_2 -> [ID , DATE]
                    Karray *karray = kson_array_parse(response->get(response,"DATA_2"));
                    query = kmemory_print_string("UPDATE MESSAGES SET MODE='2' WHERE `CONNECTION_ID`='%s' AND `DATE`='%s' AND `PEER_ID`='%s'",
                                                 response->get(response,"CONNECTION_ID"),
                                                 karray->get(karray,1),
                                                 karray->get(karray,0)
                    );
                    sqlite3_exec(interface_->database,query,0,0,NULL);
                    kmemory_free(query);
                    karray_free(karray);
                }
            }else{
                // TEXT , STICKER , LOCATION , CONTACT , STREAM , LOG
                query = kmemory_print_string("INSERT INTO MESSAGES(`PEER_ID`,`CONNECTION_ID`,`MODE`,`MESSAGE_TYPE`,`DATE`,`FORWARD_ID`,`REPLY_DATE`,`DATA_1`,`DATA_2`,`NOT_SEEN`) VALUES('%s','%s','%s','%s','%s','%s','%s','%s','%s','%s')",
                                             response->get(response,"PEER_ID"),
                                             response->get(response,"CONNECTION_ID"),
                                             response->get(response,"MODE"),
                                             response->get(response,"MESSAGE_TYPE"),
                                             response->get(response,"DATE"),
                                             response->get(response,"FORWARD_ID"),
                                             response->get(response,"REPLY_DATE"),
                                             response->get(response,"DATA_1"),
                                             response->get(response,"DATA_2"),
                                             "1"
                );
                sqlite3_exec(interface_->database,query,0,0,NULL);
                kmemory_free(query);

                if(strcmp(response->get(response,"MESSAGE_TYPE"),"STREAM") == 0) {
                    Kmap *link_kmap = interface_->stream->decode(interface_->stream,response->get(response,"DATA_1"));
                    if(link_kmap == NULL){
                        return 0;
                    }
                    char *mime = link_kmap->get(link_kmap,"MIME");
                    if(mime == NULL){
                        kmap_free(link_kmap);
                        return 0;
                    }
                    if(strcmp(response->get(response,"PEER_ID"),interface_self_get(interface,"SETTING_ID")) == 0){
                        // set
                        Karray *karray = kson_array_parse(interface_self_get(interface,"STREAM_UPLOAD"));
                        if(karray == NULL){
                            return 0;
                        }
                        for(int cursor = 0 ; cursor < karray->length(karray) ; cursor++){
                            if(strstr(mime,karray->get(karray,cursor)) != NULL){
                                // should start set stream
                                interface_->onInterface(interface,INTERFACE_STREAMSET,response);
                                break;
                            }
                        }
                    }else{
                        // get
                        Karray *karray = kson_array_parse(interface_self_get(interface,"STREAM_DOWNLOAD"));
                        if(karray == NULL){
                            return 0;
                        }
                        for(int cursor = 0 ; cursor < karray->length(karray) ; cursor++){
                            if(strstr(mime,karray->get(karray,cursor)) != NULL){
                                // should start get stream
                                interface_->onInterface(interface,INTERFACE_STREAMGET,response);
                                break;
                            }
                        }
                    }
                    kmap_free(link_kmap);
                }
            }
        }else{
            // response no
            if(strcmp(response->get(response,"ERROR"),"TIMEOUT") == 0){
                query = kmemory_print_string("UPDATE MESSAGES SET `MODE`='-1' WHERE `CONNECTION_ID`='%s' AND `DATE`='%s'",
                                             request->get(request,"CONNECTION_ID"),
                                             response->get(response,"TOKEN")
                );
                sqlite3_exec(interface_->database,query,0,0,NULL);
                kmemory_free(query);
            }else{
                query = kmemory_print_string("DELETE FROM MESSAGES WHERE `CONNECTION_ID`='%s' AND `DATE`='%s'",
                                             request->get(request,"CONNECTION_ID"),
                                             response->get(response,"TOKEN")
                );
                sqlite3_exec(interface_->database,query,0,0,NULL);
                kmemory_free(query);
            }
        }
        return 0;
    }
}
int interface_handle_protocol_message_handle(struct Interface *interface , Kmap *request , Kmap *response){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(response == NULL){
        // request
        return 1;
    }else{
        // response
        if(response->indexof(response,"ERROR") < 0){
            // response ok
        }else{
            // response no
            if(strcmp(response->get(response,"HANDLE"),"PEER_REFRESH") == 0){
                interface->protocol_peer_load(interface,NULL,NULL,response->get(response,"PEER_ID"),"PEER_INFOS",NULL,NULL,NULL,0,0);
            }else if(strcmp(response->get(response,"HANDLE"),"CONNECTION_REFRESH") == 0){
                interface->protocol_peer_load(interface,NULL,NULL,response->get(response,"CONNECTION_ID"),"CONNECTION_INFOS",NULL,NULL,NULL,0,0);
            }
        }
        return 0;
    }
}

Kmap* interface_protocol_request(struct Interface *interface , ProtocolType type , ...){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return NULL;
    }
    va_list variable_list;
    va_start(variable_list,interface_->protocol->request_size(interface_->protocol,type));
    Kmap *request = interface_->protocol->request(interface_->protocol,type, variable_list);
    va_end(variable_list);
    return request;
}
int interface_protocol_update(struct Interface *interface , void *data, char *token  , void (*destructor)(void *old_data)){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    InterfaceItem *item = interface_->items->get(interface_->items,token);
    if(item == NULL){
        return 0;
    }
    if(destructor != NULL){
        destructor(item->data);
    }
    item->data = data;
    return 1;
}
int interface_protocol_peer_captcha(struct Interface *interface , void *data , char *token , char *phone , char *type){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(interface->state(interface) == INTERFACE_STATE_STOP){
        return 0;
    }
    InterfaceItem *item = kmemory_alloc(sizeof(InterfaceItem));
    item->data = data;
    item->request = interface_->protocol->peer_captcha(interface_->protocol,token,phone,type);
    if(item->request == NULL){
        return 0;
    }
    return interface_->items->put(interface_->items,kmemory_copy_string(item->request->get(item->request,"TOKEN")),KTYPE_HEAP,item,KTYPE_HEAP);
}
int interface_protocol_peer_login(struct Interface *interface , void *data , char *token , char *id , char *scode , char *platform , char *version){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(interface->state(interface) == INTERFACE_STATE_STOP){
        return 0;
    }
    InterfaceItem *item = kmemory_alloc(sizeof(InterfaceItem));
    item->data = data;
    item->request = interface_->protocol->peer_login(interface_->protocol,token,id,scode,platform,version);
    if(item->request == NULL){
        return 0;
    }
    return interface_->items->put(interface_->items,kmemory_copy_string(item->request->get(item->request,"TOKEN")),KTYPE_HEAP,item,KTYPE_HEAP);
}
int interface_protocol_peer_logout(struct Interface *interface , void *data , char *token){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(interface->state(interface) == INTERFACE_STATE_STOP){
        return 0;
    }
    InterfaceItem *item = kmemory_alloc(sizeof(InterfaceItem));
    item->data = data;
    item->request = interface_->protocol->peer_logout(interface_->protocol,token);
    if(item->request == NULL){
        return 0;
    }
    return interface_->items->put(interface_->items,kmemory_copy_string(item->request->get(item->request,"TOKEN")),KTYPE_HEAP,item,KTYPE_HEAP);
}
int interface_protocol_peer_create(struct Interface *interface , void *data , char *token , char *phone , char *captcha){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(interface->state(interface) == INTERFACE_STATE_STOP){
        return 0;
    }
    InterfaceItem *item = kmemory_alloc(sizeof(InterfaceItem));
    item->data = data;
    item->request = interface_->protocol->peer_create(interface_->protocol,token,phone,captcha);
    if(item->request == NULL){
        return 0;
    }
    return interface_->items->put(interface_->items,kmemory_copy_string(item->request->get(item->request,"TOKEN")),KTYPE_HEAP,item,KTYPE_HEAP);
}
int interface_protocol_peer_delete(struct Interface *interface , void *data , char *token){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(interface->state(interface) == INTERFACE_STATE_STOP){
        return 0;
    }
    InterfaceItem *item = kmemory_alloc(sizeof(InterfaceItem));
    item->data = data;
    item->request = interface_->protocol->peer_delete(interface_->protocol,token);
    if(item->request == NULL){
        return 0;
    }
    return interface_->items->put(interface_->items,kmemory_copy_string(item->request->get(item->request,"TOKEN")),KTYPE_HEAP,item,KTYPE_HEAP);
}
int interface_protocol_peer_load(struct Interface *interface , void *data , char *token , char *id , char *type , char *mode , char *param_1 , char *param_2 , int local , int try){
    /* TYPES :
     *      PEER_INFOS
     *      PEER_PICTURES
     *      PEER_PEERS
     *      PEER_CONNECTIONS
     *
     *      CONNECTION_INFOS
     *      CONNECTION_PEERS
     *      CONNECTION_PEERS
     *
     *      MESSAGES
     */
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(interface->state(interface) == INTERFACE_STATE_STOP){
        return 0;
    }
    if(local){
        return interface_protocol_peer_load_local(interface,data,token,id,type,mode,param_1,param_2,try);
    }else{
        return interface_protocol_peer_load_server(interface,data,token,id,type,mode,param_1,param_2,try);
    }
}
int interface_protocol_peer_load_local(struct Interface *interface , void *data , char *token , char *id , char *type , char *mode , char *param_1 , char *param_2 , int try){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(interface->state(interface) == INTERFACE_STATE_STOP){
        return 0;
    }

    // onRequest
    char *request_text = kmemory_print_string("{\"DATA_TYPE\":\"%s\",\"TOKEN\":\"%s\",\"ID\":\"%s\",\"TYPE\":\"%s\",\"MODE\":\"%s\",\"PARAM_1\":\"%s\",\"PARAM_2\":\"%s\"}",
                                              "PEER_LOAD",
                                              token == NULL ? "LOCAL" : token,
                                              id,
                                              type,
                                              mode,
                                              param_1,
                                              param_2
    );
    Kmap *request = kson_parse(request_text);
    kmemory_free(request_text);
    interface_->onProtocol(interface,PROTOCOL_PEER_LOAD,data,token == NULL ? "LOCAL" : token,request,NULL);

    char *query = NULL;
    if(strcmp(type,"PEER_INFOS") == 0){
        query = kmemory_print_string("SELECT INFOS FROM PEERS WHERE ID='%s'",
                                     id
        );
        sqlite3_stmt *statement;
        sqlite3_prepare(interface_->database,query,-1,&statement,NULL);
        kmemory_free(query);
        if(sqlite3_step(statement) != SQLITE_DONE){
            char *value_text = (char *) sqlite3_column_text(statement,0);
            // onLoad
            char *load_text = kmemory_print_string("{\"DATA_TYPE\":\"%s\",\"TOKEN\":\"%s\",\"VALUE\":\"%s\"}",
                                                   "PEER_LOAD",
                                                   token == NULL ? "LOCAL" : token,
                                                   value_text
            );
            Kmap *load = kson_parse(load_text);
            kmemory_free(load_text);
            interface_->onProtocol(interface,PROTOCOL_PEER_LOAD,data,token == NULL ? "LOCAL" : token,request,load);
            kmap_free(load);
        }else if(try){
            sqlite3_finalize(statement);
            return interface_protocol_peer_load_server(interface,data,token,id,type,mode,param_1,param_2,0);
        }
        sqlite3_finalize(statement);
    }else if(strcmp(type,"PEER_PICTURES") == 0){
        query = kmemory_print_string("SELECT PICTURES FROM PEERS WHERE ID='%s'",
                                     id
        );
        sqlite3_stmt *statement;
        sqlite3_prepare(interface_->database,query,-1,&statement,NULL);
        kmemory_free(query);
        if(sqlite3_step(statement) != SQLITE_DONE){
            char *value_text = (char *) sqlite3_column_text(statement,0);
            // onLoad
            char *load_text = kmemory_print_string("{\"DATA_TYPE\":\"%s\",\"TOKEN\":\"%s\",\"VALUE\":\"%s\"}",
                                                   "PEER_LOAD",
                                                   token == NULL ? "LOCAL" : token,
                                                   value_text
            );
            Kmap *load = kson_parse(load_text);
            kmemory_free(load_text);
            interface_->onProtocol(interface,PROTOCOL_PEER_LOAD,data,token == NULL ? "LOCAL" : token,request,load);
            kmap_free(load);
        }else if(try){
            sqlite3_finalize(statement);
            return interface_protocol_peer_load_server(interface,data,token,id,type,mode,param_1,param_2,0);
        }
        sqlite3_finalize(statement);
    }else if(strcmp(type,"PEER_PEERS") == 0){
        query = kmemory_print_string("SELECT PICTURES FROM PEERS WHERE ID='%s'",
                                     id
        );
        sqlite3_stmt *statement;
        sqlite3_prepare(interface_->database,query,-1,&statement,NULL);
        kmemory_free(query);
        if(sqlite3_step(statement) != SQLITE_DONE){
            char *value_text = (char *) sqlite3_column_text(statement,0);
            // onLoad
            char *load_text = kmemory_print_string("{\"DATA_TYPE\":\"%s\",\"TOKEN\":\"%s\",\"VALUE\":\"%s\"}",
                                                   "PEER_LOAD",
                                                   token == NULL ? "LOCAL" : token,
                                                   value_text
            );
            Kmap *load = kson_parse(load_text);
            kmemory_free(load_text);
            interface_->onProtocol(interface,PROTOCOL_PEER_LOAD,data,token == NULL ? "LOCAL" : token,request,load);
            kmap_free(load);
        }else if(try){
            sqlite3_finalize(statement);
            return interface_protocol_peer_load_server(interface,data,token,id,type,mode,param_1,param_2,0);
        }
        sqlite3_finalize(statement);
    }else if(strcmp(type,"PEER_CONNECTIONS") == 0){
        query = kmemory_print_string("SELECT CONNECTIONS FROM PEERS WHERE ID='%s'",
                                     id
        );
        sqlite3_stmt *statement;
        sqlite3_prepare(interface_->database,query,-1,&statement,NULL);
        kmemory_free(query);
        if(sqlite3_step(statement) != SQLITE_DONE){
            char *value_text = (char *) sqlite3_column_text(statement,0);
            // onLoad
            char *load_text = kmemory_print_string("{\"DATA_TYPE\":\"%s\",\"TOKEN\":\"%s\",\"VALUE\":\"%s\"}",
                                                   "PEER_LOAD",
                                                   token == NULL ? "LOCAL" : token,
                                                   value_text
            );
            Kmap *load = kson_parse(load_text);
            kmemory_free(load_text);
            interface_->onProtocol(interface,PROTOCOL_PEER_LOAD,data,token == NULL ? "LOCAL" : token,request,load);
            kmap_free(load);
        }else if(try){
            sqlite3_finalize(statement);
            return interface_protocol_peer_load_server(interface,data,token,id,type,mode,param_1,param_2,0);
        }
        sqlite3_finalize(statement);
    }else if(strcmp(type,"CONNECTION_INFOS") == 0){
        query = kmemory_print_string("SELECT LINK,NAME,BIO FROM CONNECTIONS WHERE ID='%s'",
                                     id
        );
        sqlite3_stmt *statement;
        sqlite3_prepare(interface_->database,query,-1,&statement,NULL);
        kmemory_free(query);
        if(sqlite3_step(statement) != SQLITE_DONE){
            char *value_text = (char *) sqlite3_column_text(statement,0);
            // onLoad
            char *load_text = kmemory_print_string("{\"DATA_TYPE\":\"%s\",\"TOKEN\":\"%s\",\"VALUE\":\"%s\"}",
                                                   "PEER_LOAD",
                                                   token == NULL ? "LOCAL" : token,
                                                   value_text
            );

            Kmap *load = kson_parse(load_text);
            kmemory_free(load_text);
            interface_->onProtocol(interface,PROTOCOL_PEER_LOAD,data,token == NULL ? "LOCAL" : token,request,load);
            kmap_free(load);
        }else if(try){
            sqlite3_finalize(statement);
            return interface_protocol_peer_load_server(interface,data,token,id,type,mode,param_1,param_2,0);
        }
        sqlite3_finalize(statement);
    }else if(strcmp(type,"CONNECTION_PICTURES") == 0){
        query = kmemory_print_string("SELECT PICTURES FROM CONNECTIONS WHERE ID='%s'",
                                     id
        );
        sqlite3_stmt *statement;
        sqlite3_prepare(interface_->database,query,-1,&statement,NULL);
        kmemory_free(query);
        if(sqlite3_step(statement) != SQLITE_DONE){
            char *value_text = (char *) sqlite3_column_text(statement,0);
            // onLoad
            char *load_text = kmemory_print_string("{\"DATA_TYPE\":\"%s\",\"TOKEN\":\"%s\",\"VALUE\":\"%s\"}",
                                                   "PEER_LOAD",
                                                   token == NULL ? "LOCAL" : token,
                                                   value_text
            );

            Kmap *load = kson_parse(load_text);
            kmemory_free(load_text);
            interface_->onProtocol(interface,PROTOCOL_PEER_LOAD,data,token == NULL ? "LOCAL" : token,request,load);
            kmap_free(load);
        }else if(try){
            sqlite3_finalize(statement);
            return interface_protocol_peer_load_server(interface,data,token,id,type,mode,param_1,param_2,0);
        }
        sqlite3_finalize(statement);
    }else if(strcmp(type,"CONNECTION_PEERS") == 0){
        query = kmemory_print_string("SELECT PEERS FROM CONNECTIONS WHERE ID='%s'",
                                     id
        );
        sqlite3_stmt *statement;
        sqlite3_prepare(interface_->database,query,-1,&statement,NULL);
        kmemory_free(query);
        if(sqlite3_step(statement) != SQLITE_DONE){
            char *value_text = (char *) sqlite3_column_text(statement,0);
            // onLoad
            char *load_text = kmemory_print_string("{\"DATA_TYPE\":\"%s\",\"TOKEN\":\"%s\",\"VALUE\":\"%s\"}",
                                                   "PEER_LOAD",
                                                   token == NULL ? "LOCAL" : token,
                                                   value_text
            );

            Kmap *load = kson_parse(load_text);
            kmemory_free(load_text);
            interface_->onProtocol(interface,PROTOCOL_PEER_LOAD,data,token == NULL ? "LOCAL" : token,request,load);
            kmap_free(load);
        }else if(try){
            sqlite3_finalize(statement);
            return interface_protocol_peer_load_server(interface,data,token,id,type,mode,param_1,param_2,0);
        }
        sqlite3_finalize(statement);
    }else if(strcmp(type,"MESSAGES") == 0){
        if(strcmp(id,"") == 0){
            // load titles
            query = kmemory_print_string("SELECT PEER_ID,CONNECTION_ID,MODE,MESSAGE_TYPE,MAX(`DATE`) AS `DATE,FORWARD_ID,REPLY_DATE,DATA_1,DATA_2,SUM(`NOT_SEEN`) AS `NOT_SEEN` FROM MESSAGES GROUP BY `CONNECTION_ID`");
        }else{
            if(strcmp(mode,"0") == 0){
                query = kmemory_print_string("SELECT PEER_ID,CONNECTION_ID,MODE,MESSAGE_TYPE,DATE,FORWARD_ID,REPLY_DATE,DATA_1,DATA_2,NOT_SEEN FROM MESSAGES WHERE CONNECTION_ID='%s' AND DATE>'%s' AND DATE<='%s' ORDER BY DATE DESC",
                                             id,
                                             param_1,
                                             param_2
                );
            }else if(strcmp(mode,"1") == 0){
                query = kmemory_print_string("SELECT PEER_ID,CONNECTION_ID,MODE,MESSAGE_TYPE,DATE,FORWARD_ID,REPLY_DATE,DATA_1,DATA_2,NOT_SEEN FROM MESSAGES WHERE CONNECTION_ID='%s' AND DATE>'%s' ORDER BY DATE DESC",
                                             id,
                                             param_1
                );
            }else if(strcmp(mode,"2") == 0){
                query = kmemory_print_string("SELECT PEER_ID,CONNECTION_ID,MODE,MESSAGE_TYPE,DATE,FORWARD_ID,REPLY_DATE,DATA_1,DATA_2,NOT_SEEN FROM MESSAGES WHERE CONNECTION_ID='%s' AND DATE<='%s' ORDER BY DATE DESC",
                                             id,
                                             param_1
                );
            }else if(strcmp(mode,"3") == 0){
                query = kmemory_print_string("SELECT PEER_ID,CONNECTION_ID,MODE,MESSAGE_TYPE,DATE,FORWARD_ID,REPLY_DATE,DATA_1,DATA_2,NOT_SEEN FROM MESSAGES WHERE CONNECTION_ID='%s' AND DATE>'%s' ORDER BY DATE DESC LIMIT %s",
                                             id,
                                             param_1,
                                             param_2
                );
            }else{
                query = kmemory_print_string("SELECT PEER_ID,CONNECTION_ID,MODE,MESSAGE_TYPE,DATE,FORWARD_ID,REPLY_DATE,DATA_1,DATA_2,NOT_SEEN FROM MESSAGES WHERE CONNECTION_ID='%s' AND DATE<='%s' ORDER BY DATE ASC LIMIT %s",
                                             id,
                                             param_1,
                                             param_2
                );
            }
        }
        sqlite3_stmt *statement;
        sqlite3_prepare(interface_->database,query,-1,&statement,NULL);
        kmemory_free(query);
        while(sqlite3_step(statement) != SQLITE_DONE){
            char *message_peer_id = (char *) sqlite3_column_text(statement,0);
            char *message_connection_id = (char *) sqlite3_column_text(statement,1);
            char *message_mode = (char *) sqlite3_column_text(statement,2);
            char *message_message_type = (char *) sqlite3_column_text(statement,3);
            char *message_date = (char *) sqlite3_column_text(statement,4);
            char *message_forward_id = (char *) sqlite3_column_text(statement,5);
            char *message_reply_date = (char *) sqlite3_column_text(statement,6);
            char *message_data_1 = (char *) sqlite3_column_text(statement,7);
            char *message_data_2 = (char *) sqlite3_column_text(statement,8);
            char *message_not_seen = (char *) sqlite3_column_text(statement,9);
            // onLoad
            Kmap *value = kson_parse("{}");
            value->put(value,"PEER_ID",KTYPE_STACK,message_peer_id,KTYPE_STACK);
            value->put(value,"CONNECTION_ID",KTYPE_STACK,message_connection_id,KTYPE_STACK);
            value->put(value,"MODE",KTYPE_STACK,message_mode,KTYPE_STACK);
            value->put(value,"MESSAGE_TYPE",KTYPE_STACK,message_message_type,KTYPE_STACK);
            value->put(value,"DATE",KTYPE_STACK,message_date,KTYPE_STACK);
            value->put(value,"FORWARD_ID",KTYPE_STACK,message_forward_id,KTYPE_STACK);
            value->put(value,"REPLY_DATE",KTYPE_STACK,message_reply_date,KTYPE_STACK);
            value->put(value,"DATA_1",KTYPE_STACK,message_data_1,KTYPE_STACK);
            value->put(value,"DATA_2",KTYPE_STACK,message_data_2,KTYPE_STACK);
            value->put(value,"NOT_SEEN",KTYPE_STACK,message_not_seen,KTYPE_STACK);
            char *value_text = kson_pack(value);
            kmap_free(value);
            char *load_text = kmemory_print_string("{\"DATA_TYPE\":\"%s\",\"TOKEN\":\"%s\",\"VALUE\":\"%s\"}",
                                                   "PEER_LOAD",
                                                   token == NULL ? "LOCAL" : token,
                                                   value_text
            );

            Kmap *load = kson_parse(load_text);
            kmemory_free(load_text);
            kmemory_free(value_text);
            interface_->onProtocol(interface,PROTOCOL_PEER_LOAD,data,token == NULL ? "LOCAL" : token,request,load);
            kmap_free(load);
        }
        sqlite3_finalize(statement);
    }

    // onResponse
    char *response_text = kmemory_print_string("{\"DATA_TYPE\":\"%s\",\"TOKEN\":\"%s\"}",
                                               "PEER_LOAD",
                                               token == NULL ? "LOCAL" : token
    );
    Kmap *response = kson_parse(response_text);
    kmemory_free(response_text);
    interface_->onProtocol(interface,PROTOCOL_PEER_LOAD,data,token == NULL ? "LOCAL" : token,request,response);

    kmap_free(request);
    kmap_free(response);
    return 1;
}
int interface_protocol_peer_load_server(struct Interface *interface , void *data , char *token , char *id , char *type , char *mode , char *param_1 , char *param_2 , int try){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(interface->state(interface) == INTERFACE_STATE_STOP){
        return 0;
    }
    if(interface_state(interface) == INTERFACE_STATE_ONLINE){
        InterfaceItem *item = kmemory_alloc(sizeof(InterfaceItem));
        item->data = data;
        item->request = interface_->protocol->peer_load(interface_->protocol,token,id,type,mode,param_1,param_2);
        return interface_->items->put(interface_->items,kmemory_copy_string(item->request->get(item->request,"TOKEN")),KTYPE_HEAP,item,KTYPE_HEAP);
    }else{
        if(try){
            return interface_protocol_peer_load_local(interface,data,token,id,type,mode,param_1,param_2,0);
        }else{
            return 0;
        }
    }
}
int interface_protocol_peer_set(struct Interface *interface , void *data , char *token , char *id , char *key , char *option , char *value , char *operator){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(interface->state(interface) == INTERFACE_STATE_STOP){
        return 0;
    }
    InterfaceItem *item = kmemory_alloc(sizeof(InterfaceItem));
    item->data = data;
    item->request = interface_->protocol->peer_set(interface_->protocol,token,id,key,option,value,operator);
    if(item->request == NULL){
        return 0;
    }
    return interface_->items->put(interface_->items,kmemory_copy_string(item->request->get(item->request,"TOKEN")),KTYPE_HEAP,item,KTYPE_HEAP);
}
int interface_protocol_peer_id(struct Interface *interface , void *data , char *token , char *key , char *type){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(interface->state(interface) == INTERFACE_STATE_STOP){
        return 0;
    }
    InterfaceItem *item = kmemory_alloc(sizeof(InterfaceItem));
    item->data = data;
    item->request = interface_->protocol->peer_id(interface_->protocol,token,key,type);
    if(item->request == NULL){
        return 0;
    }
    return interface_->items->put(interface_->items,kmemory_copy_string(item->request->get(item->request,"TOKEN")),KTYPE_HEAP,item,KTYPE_HEAP);
}
int interface_protocol_connection_create(struct Interface *interface , void *data , char *token){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(interface->state(interface) == INTERFACE_STATE_STOP){
        return 0;
    }
    InterfaceItem *item = kmemory_alloc(sizeof(InterfaceItem));
    item->data = data;
    item->request = interface_->protocol->connection_create(interface_->protocol,token);
    if(item->request == NULL){
        return 0;
    }
    return interface_->items->put(interface_->items,kmemory_copy_string(item->request->get(item->request,"TOKEN")),KTYPE_HEAP,item,KTYPE_HEAP);
}
int interface_protocol_connection_delete(struct Interface *interface , void *data , char *token , char *id){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(interface->state(interface) == INTERFACE_STATE_STOP){
        return 0;
    }
    InterfaceItem *item = kmemory_alloc(sizeof(InterfaceItem));
    item->data = data;
    item->request = interface_->protocol->connection_delete(interface_->protocol,token,id);
    if(item->request == NULL){
        return 0;
    }
    return interface_->items->put(interface_->items,kmemory_copy_string(item->request->get(item->request,"TOKEN")),KTYPE_HEAP,item,KTYPE_HEAP);
}
int interface_protocol_connection_set(struct Interface *interface , void *data , char *token , char *id , char *key , char *option , char *value , char *operator){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(interface->state(interface) == INTERFACE_STATE_STOP){
        return 0;
    }
    InterfaceItem *item = kmemory_alloc(sizeof(InterfaceItem));
    item->data = data;
    item->request = interface_->protocol->connection_set(interface_->protocol,token,id,key,option,value,operator);
    if(item->request == NULL){
        return 0;
    }
    return interface_->items->put(interface_->items,kmemory_copy_string(item->request->get(item->request,"TOKEN")),KTYPE_HEAP,item,KTYPE_HEAP);
}
int interface_protocol_connection_id(struct Interface *interface , void *data , char *token , char *key , char *type){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(interface->state(interface) == INTERFACE_STATE_STOP){
        return 0;
    }
    InterfaceItem *item = kmemory_alloc(sizeof(InterfaceItem));
    item->data = data;
    item->request = interface_->protocol->connection_id(interface_->protocol,token,key,type);
    if(item->request == NULL){
        return 0;
    }
    return interface_->items->put(interface_->items,kmemory_copy_string(item->request->get(item->request,"TOKEN")),KTYPE_HEAP,item,KTYPE_HEAP);
}
int interface_protocol_message_set(struct Interface *interface , void *data , char *token , char *conection_id , char *message_type , char *forward_id , char *reply_date , char *data_1 , char *data_2){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(interface->state(interface) == INTERFACE_STATE_STOP){
        return 0;
    }
    InterfaceItem *item = kmemory_alloc(sizeof(InterfaceItem));
    item->data = data;
    item->request = interface_->protocol->message_set(interface_->protocol,token,conection_id,message_type,forward_id,reply_date,data_1,data_2);
    if(item->request == NULL){
        return 0;
    }
    return interface_->items->put(interface_->items,kmemory_copy_string(item->request->get(item->request,"TOKEN")),KTYPE_HEAP,item,KTYPE_HEAP);
}
int interface_protocol_message_handle(struct Interface *interface , void *data , char *token , char *conection_id , char *handle){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(interface->state(interface) == INTERFACE_STATE_STOP){
        return 0;
    }
    InterfaceItem *item = kmemory_alloc(sizeof(InterfaceItem));
    item->data = data;
    item->request = interface_->protocol->message_handle(interface_->protocol,token,conection_id,handle);
    if(item->request == NULL){
        return 0;
    }
    return interface_->items->put(interface_->items,kmemory_copy_string(item->request->get(item->request,"TOKEN")),KTYPE_HEAP,item,KTYPE_HEAP);
}


int interface_stream_update(struct Interface *interface , void *data, char *link  , void (*destructor)(void *old_data)){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    InterfaceItem *item = interface_->items->get(interface_->items,link);
    if(item == NULL){
        return 0;
    }
    if(destructor != NULL){
        destructor(item->data);
    }
    item->data = data;
    return 1;
}
int interface_stream_set(struct Interface *interface , void *data , char *link , char *path , int calltime){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(interface->state(interface) == INTERFACE_STATE_STOP){
        return 0;
    }
    InterfaceItem *item = kmemory_alloc(sizeof(InterfaceItem));
    item->request = NULL;
    item->data = data;
    interface_->items->put(interface_->items,kmemory_copy_string(link),KTYPE_HEAP,item,KTYPE_HEAP);
    return interface_->stream->set(interface_->stream,link,path,calltime);
}
int interface_stream_get(struct Interface *interface , void *data , char *link , char *path , int calltime){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    if(interface->state(interface) == INTERFACE_STATE_STOP){
        return 0;
    }
    InterfaceItem *item = kmemory_alloc(sizeof(InterfaceItem));
    item->request = NULL;
    item->data = data;
    interface_->items->put(interface_->items,kmemory_copy_string(link),KTYPE_HEAP,item,KTYPE_HEAP);
    return interface_->stream->get(interface_->stream,link,path,calltime);
}
Kmap* interface_stream_sets(struct Interface *interface){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return NULL;
    }
    return interface_->stream->sets(interface_->stream);
}
Kmap* interface_stream_gets(struct Interface *interface){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return NULL;
    }
    return interface_->stream->gets(interface_->stream);
}
int interface_stream_cancel(struct Interface *interface , char *link){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    return interface_->stream->cancel(interface_->stream,link);
}
Kmap* interface_stream_state(struct Interface *interface , char *link){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return NULL;
    }
    StreamPipe *pipe = interface_->stream->state(interface_->stream,link);
    if(pipe == NULL){
        return NULL;
    }
    Kmap *result = kson_parse("{}");
    result->put(result,"SIZE",KTYPE_STACK,kmemory_copy_long(pipe->size),KTYPE_HEAP);
    result->put(result,"SEEK",KTYPE_STACK,kmemory_copy_long(pipe->seek),KTYPE_HEAP);
    result->put(result,"TYPE",KTYPE_STACK,pipe->type == STREAM_PIPE_TYPE_SET ? "SET" : "GET",KTYPE_STACK);
    return result;
}
int interface_stream_clear(struct Interface *interface){
    // clear folder
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    char *path = kmemory_print_string("%s/%s",interface_->interface_path,".Stream");
    DIR *directory;
    struct dirent *directory_item;
    directory = opendir(path);
    while((directory_item = readdir(directory)) != NULL){
        if(directory_item->d_type != 4){
            char *file_path = kmemory_print_string("%s/%s/%s",interface_->interface_path,".Stream",directory_item->d_name);
            remove(file_path);
            kmemory_free(file_path);
        }
    }
    closedir(directory);
    kmemory_free(path);
    return 1;
}
long interface_stream_size(struct Interface *interface){
    // get folder size
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return -1;
    }
    char *path = kmemory_print_string("%s/%s",interface_->interface_path,".Stream");
    struct stat state;
    DIR *directory;
    struct dirent *directory_item;
    long size = 0;
    directory = opendir(path);
    while((directory_item = readdir(directory)) != NULL){
        if(directory_item->d_type != 4){
            char *file_path = kmemory_print_string("%s/%s/%s",interface_->interface_path,".Stream",directory_item->d_name);
            stat(file_path , &state);
            size += state.st_size;
            kmemory_free(file_path);
        }
    }
    closedir(directory);
    kmemory_free(path);
    return size;
}
char* interface_stream_getdata(struct Interface *interface , char *link){
    // select data_2 from database where link = link
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return NULL;
    }
    char *query = kmemory_print_string("SELECT DATA_2 FROM MESSAGES WHERE MESSAGE_TYPE='STREAM' AND DATA_1='%s'",link);
    sqlite3_stmt *statement;
    sqlite3_prepare(interface_->database,query,-1,&statement,NULL);
    kmemory_free(query);
    if(sqlite3_step(statement) == SQLITE_DONE){
        sqlite3_finalize(statement);
        return NULL;
    }
    char *result = (char *) sqlite3_column_text(statement,0);
    sqlite3_finalize(statement);
    return result;
}
int interface_stream_setdata(struct Interface *interface , char *link , char *state){
    // update database where link = link -> data_2 = state
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    char *query = kmemory_print_string("UPDATE MESSAGES SET DATA_2='%s' WHERE MESSAGE_TYPE='STREAM' AND DATA_1='%s'",state,link);
    sqlite3_exec(interface_->database,query,0,0,NULL);
    kmemory_free(query);
    return 1;
}
char* interface_stream_file(struct Interface *interface , char *link , int recreate){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return NULL;
    }
    Kmap *link_kmap = interface_->stream->decode(interface_->stream,link);
    if(link_kmap == NULL){
        return NULL;
    }
    if(link_kmap->indexof(link_kmap,"SIZE") < 0 || link_kmap->indexof(link_kmap,"CHECKSUM") < 0){
        kmap_free(link_kmap);
        return NULL;
    }
    char *path = kmemory_print_string("%s/%s/%s%s",interface_->interface_path,".Stream",(char *) link_kmap->get(link_kmap, "SIZE"),(char *) link_kmap->get(link_kmap, "CHECKSUM"));
    kmap_free(link_kmap);
    if(recreate){
        fclose(fopen(path,"w+"));
    }else{
        fclose(fopen(path,"ab+"));
    }
    return path;
}
char* interface_stream_temp(struct Interface *interface , int recreate){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return NULL;
    }
    char *path = kmemory_print_string("%s/%s/%s",interface_->interface_path,".Data","temp");
    if(recreate){
        fclose(fopen(path,"w+"));
    }else{
        fclose(fopen(path,"ab+"));
    }
    return path;
}
char* interface_stream_encode(struct Interface *interface , char *name , char *size , char *mime , char *checksum){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return NULL;
    }
    Kmap *kmap = kson_parse("{}");
    kmap->put(kmap,"NAME",KTYPE_STACK,name,KTYPE_STACK);
    kmap->put(kmap,"SIZE",KTYPE_STACK,size,KTYPE_STACK);
    kmap->put(kmap,"MIME",KTYPE_STACK,mime,KTYPE_STACK);
    kmap->put(kmap,"CHECKSUM",KTYPE_STACK,checksum,KTYPE_STACK);
    char *link = interface_->stream->encode(interface_->stream,kmap);
    kmap_free(kmap);
    return link;
}
Kmap* interface_stream_decode(struct Interface *interface , char *link){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return NULL;
    }
    return interface_->stream->decode(interface_->stream,link);
}


int interface_caster_tune(struct Interface *interface , char *target_id , CasterTune tune){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    return interface_->caster->tune(interface_->caster,target_id,tune);
}
int interface_caster_cast(struct Interface *interface , void *data , int size , int end){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return 0;
    }
    return interface_->caster->cast(interface_->caster,data,size,end);
}


void interface_protocol_onRequest(struct Protocol *protocol , ProtocolType type , char *token , Kmap *request){
    Interface_ *interface_ = protocol->bundle;
    if(interface_ == NULL){
        return;
    }
    InterfaceItem *item = interface_->items->get(interface_->items,token);
    if(item == NULL){
        return;
    }
    int result;
    switch (type){
        case PROTOCOL_PEER_CAPTCHA:
            result = interface_handle_protocol_peer_captcha((struct Interface *) interface_,request,NULL);
            break;
        case PROTOCOL_PEER_LOGIN:
            result = interface_handle_protocol_peer_login((struct Interface *) interface_,request,NULL);
            break;
        case PROTOCOL_PEER_LOGOUT:
            result = interface_handle_protocol_peer_logout((struct Interface *) interface_,request,NULL);
            break;
        case PROTOCOL_PEER_CREATE:
            result = interface_handle_protocol_peer_create((struct Interface *) interface_,request,NULL);
            break;
        case PROTOCOL_PEER_DELETE:
            result = interface_handle_protocol_peer_delete((struct Interface *) interface_,request,NULL);
            break;
        case PROTOCOL_PEER_LOAD:
            result = interface_handle_protocol_peer_load((struct Interface *) interface_,request,NULL);
            break;
        case PROTOCOL_PEER_SET:
            result = interface_handle_protocol_peer_set((struct Interface *) interface_,request,NULL);
            break;
        case PROTOCOL_PEER_ID:
            result = interface_handle_protocol_peer_id((struct Interface *) interface_,request,NULL);
            break;
        case PROTOCOL_CONNECTION_CREATE:
            result = interface_handle_protocol_connection_create((struct Interface *) interface_,request,NULL);
            break;
        case PROTOCOL_CONNECTION_DELETE:
            result = interface_handle_protocol_connection_delete((struct Interface *) interface_,request,NULL);
            break;
        case PROTOCOL_CONNECTION_SET:
            result = interface_handle_protocol_connection_set((struct Interface *) interface_,request,NULL);
            break;
        case PROTOCOL_CONNECTION_ID:
            result = interface_handle_protocol_connection_id((struct Interface *) interface_,request,NULL);
            break;
        case PROTOCOL_MESSAGE_SET:
            result = interface_handle_protocol_message_set((struct Interface *) interface_,request,NULL);
            break;
        default:
            result = interface_handle_protocol_message_handle((struct Interface *) interface_,request,NULL);
            break;
    }
    interface_->onProtocol((struct Interface *) interface_, type, item->data, token, item->request, NULL);
    if(!result){
        interface_->items->remove(interface_->items,token);
    }
}
void interface_protocol_onResponse(struct Protocol *protocol , ProtocolType type , char *token , Kmap *response){
    Interface_ *interface_ = protocol->bundle;
    if(interface_ == NULL){
        return;
    }
    InterfaceItem *item = interface_->items->get(interface_->items,token);
    Kmap *request = item == NULL ? NULL : item->request;
    int result;
    switch (type){
        case PROTOCOL_PEER_CAPTCHA:
            result = interface_handle_protocol_peer_captcha((struct Interface *) interface_,request,response);
            break;
        case PROTOCOL_PEER_LOGIN:
            result = interface_handle_protocol_peer_login((struct Interface *) interface_,request,response);
            break;
        case PROTOCOL_PEER_LOGOUT:
            result = interface_handle_protocol_peer_logout((struct Interface *) interface_,request,response);
            break;
        case PROTOCOL_PEER_CREATE:
            result = interface_handle_protocol_peer_create((struct Interface *) interface_,request,response);
            break;
        case PROTOCOL_PEER_DELETE:
            result = interface_handle_protocol_peer_delete((struct Interface *) interface_,request,response);
            break;
        case PROTOCOL_PEER_LOAD:
            result = interface_handle_protocol_peer_load((struct Interface *) interface_,request,response);
            break;
        case PROTOCOL_PEER_SET:
            result = interface_handle_protocol_peer_set((struct Interface *) interface_,request,response);
            break;
        case PROTOCOL_PEER_ID:
            result = interface_handle_protocol_peer_id((struct Interface *) interface_,request,response);
            break;
        case PROTOCOL_CONNECTION_CREATE:
            result = interface_handle_protocol_connection_create((struct Interface *) interface_,request,response);
            break;
        case PROTOCOL_CONNECTION_DELETE:
            result = interface_handle_protocol_connection_delete((struct Interface *) interface_,request,response);
            break;
        case PROTOCOL_CONNECTION_SET:
            result = interface_handle_protocol_connection_set((struct Interface *) interface_,request,response);
            break;
        case PROTOCOL_CONNECTION_ID:
            result = interface_handle_protocol_connection_id((struct Interface *) interface_,request,response);
            break;
        case PROTOCOL_MESSAGE_SET:
            result = interface_handle_protocol_message_set((struct Interface *) interface_,request,response);
            break;
        default:
            result = interface_handle_protocol_message_handle((struct Interface *) interface_,request,response);
            break;
    }
    interface_->onProtocol((struct Interface *) interface_, type, item == NULL ? NULL : item->data, token, request, response);
    if(!result){
        interface_->items->remove(interface_->items,token);
    }
}


void interface_stream_onStart(struct Stream *stream , char *link){
    Interface_ *interface_ = stream->bundle;
    if(interface_ == NULL){
        return;
    }
    InterfaceItem *item = interface_->items->get(interface_->items,link);
    if(item == NULL){
        return;
    }
    interface_->onStream((struct Interface *) interface_, STREAM_START, item->data, link, 0);
}
void interface_stream_onOpen(struct Stream *stream , char *link){
    Interface_ *interface_ = stream->bundle;
    if(interface_ == NULL){
        return;
    }
    InterfaceItem *item = interface_->items->get(interface_->items,link);
    if(item == NULL){
        return;
    }
    interface_->onStream((struct Interface *) interface_, STREAM_OPEN, item->data, link, 0);
}
void interface_stream_onHandshake(struct Stream *stream , char *link){
    Interface_ *interface_ = stream->bundle;
    if(interface_ == NULL){
        return;
    }
    InterfaceItem *item = interface_->items->get(interface_->items,link);
    if(item == NULL){
        return;
    }
    interface_->onStream((struct Interface *) interface_, STREAM_HANDSHAKE, item->data, link, 0);
}
void interface_stream_onData(struct Stream *stream , char *link , long seek){
    Interface_ *interface_ = stream->bundle;
    if(interface_ == NULL){
        return;
    }
    InterfaceItem *item = interface_->items->get(interface_->items,link);
    if(item == NULL){
        return;
    }
    interface_->onStream((struct Interface *) interface_, STREAM_DATA, item->data, link, seek);
}
void interface_stream_onStop(struct Stream *stream , char *link){
    Interface_ *interface_ = stream->bundle;
    if(interface_ == NULL){
        return;
    }
    InterfaceItem *item = interface_->items->get(interface_->items,link);
    if(item == NULL){
        return;
    }
    interface_->onStream((struct Interface *) interface_, STREAM_STOP, item->data, link, 0);
    interface_->items->remove(interface_->items,link);
}


void interface_caster_onTune(struct Caster *caster , char *target_id , CasterTune tune){
    Interface_ *interface_ = caster->bundle;
    if(interface_ == NULL){
        return;
    }
    interface_->onCaster((struct Interface *) interface_, CASTER_TUNE, target_id, NULL, 0, tune);
}
void interface_caster_onCast(struct Caster *caster , char *target_id , void *data , int size){
    Interface_ *interface_ = caster->bundle;
    if(interface_ == NULL){
        return;
    }
    interface_->onCaster((struct Interface *) interface_, CASTER_CAST, target_id, data, size, NULL);
}


int interface_items_key_comperator(void *key_1 , void *key_2){
    return strcmp(key_1,key_2) == 0;
}
void interface_items_key_destructor(void *key){
    kmemory_free(key);
}
void interface_items_value_destructor(void *value){
    kmemory_free(value);
}



Interface* interface_new(
        char *protocol_host,
        int protocol_port,
        StreamParallelism stream_parallelism,
        char *stream_host,
        int stream_port,
        int stream_pool,
        char *caster_host,
        int caster_port,
        int caster_pool,
        char *interface_path,
        char *self_id,
        int interface_bundles,
        void (*onProtocol)(struct Interface *interface , ProtocolType type , void *data , char *token , Kmap *request , Kmap *response),
        void (*onStream)(struct Interface *interface , StreamType type , void *data , char *link , long seek),
        void (*onCaster)(struct Interface *interface , CasterType type , char *target_id , void *data , int size , CasterTune tune),
        void (*onInterface)(struct Interface *interface , InterfaceType type , Kmap *message)
){
    Interface_ *interface_ = kmemory_alloc(sizeof(Interface_));
    interface_->interface.start = interface_start;
    interface_->interface.state = interface_state;
    interface_->interface.stop = interface_stop;
    interface_->interface.self_set = interface_self_set;
    interface_->interface.self_get = interface_self_get;
    interface_->interface.protocol_request = interface_protocol_request;
    interface_->interface.protocol_update = interface_protocol_update;
    interface_->interface.protocol_peer_captcha = interface_protocol_peer_captcha;
    interface_->interface.protocol_peer_login = interface_protocol_peer_login;
    interface_->interface.protocol_peer_logout = interface_protocol_peer_logout;
    interface_->interface.protocol_peer_create = interface_protocol_peer_create;
    interface_->interface.protocol_peer_delete = interface_protocol_peer_delete;
    interface_->interface.protocol_peer_load = interface_protocol_peer_load;
    interface_->interface.protocol_peer_set = interface_protocol_peer_set;
    interface_->interface.protocol_peer_id = interface_protocol_peer_id;
    interface_->interface.protocol_connection_create = interface_protocol_connection_create;
    interface_->interface.protocol_connection_delete = interface_protocol_connection_delete;
    interface_->interface.protocol_connection_set = interface_protocol_connection_set;
    interface_->interface.protocol_connection_id = interface_protocol_connection_id;
    interface_->interface.protocol_message_set = interface_protocol_message_set;
    interface_->interface.protocol_message_handle = interface_protocol_message_handle;
    interface_->interface.stream_update = interface_stream_update;
    interface_->interface.stream_set = interface_stream_set;
    interface_->interface.stream_get = interface_stream_get;
    interface_->interface.stream_sets = interface_stream_sets;
    interface_->interface.stream_gets = interface_stream_gets;
    interface_->interface.stream_cancel = interface_stream_cancel;
    interface_->interface.stream_state = interface_stream_state;
    interface_->interface.stream_clear = interface_stream_clear;
    interface_->interface.stream_size = interface_stream_size;
    interface_->interface.stream_getdata = interface_stream_getdata;
    interface_->interface.stream_setdata = interface_stream_setdata;
    interface_->interface.stream_file = interface_stream_file;
    interface_->interface.stream_temp = interface_stream_temp;
    interface_->interface.stream_encode = interface_stream_encode;
    interface_->interface.stream_decode = interface_stream_decode;
    interface_->interface.caster_tune = interface_caster_tune;
    interface_->interface.caster_cast = interface_caster_cast;
    interface_->interface.bundles = kmemory_alloc(sizeof(void*)*interface_bundles);
    for(int cursor = 0 ; cursor < interface_bundles ; cursor++){
        interface_->interface.bundles[cursor] = NULL;
    }

    interface_->database = NULL;
    interface_->interface_path = kmemory_copy_string(interface_path);
    interface_->self_id = kmemory_copy_string(self_id);
    interface_->onProtocol = onProtocol;
    interface_->onStream = onStream;
    interface_->onCaster = onCaster;
    interface_->onInterface = onInterface;
    interface_->protocol = protocol_new(protocol_host,protocol_port,interface_protocol_onRequest,interface_protocol_onResponse);
    interface_->stream = stream_new(stream_parallelism,1024,stream_pool,stream_host,stream_port,interface_stream_onStart,interface_stream_onOpen,interface_stream_onHandshake,interface_stream_onData,interface_stream_onStop);
    interface_->caster = caster_new(caster_pool,1024,caster_host,caster_port,interface_caster_onTune,interface_caster_onCast);
    interface_->self = interface_self_load((struct Interface *) interface_);
    interface_->items = kmap_new(KCONCURRENCY_CONCURRENT,2.0f,interface_items_key_comperator,interface_items_key_destructor,interface_items_value_destructor);
    if(!interface_self_init((struct Interface *) interface_) || interface_->protocol == NULL || interface_->stream == NULL || interface_->caster == NULL || interface_->self == NULL || interface_->items == NULL){
        interface_free((Interface *) interface_);
        return NULL;
    }
    interface_->protocol->bundle = interface_;
    interface_->stream->bundle = interface_;
    interface_->caster->bundle = interface_;
    return (Interface *) interface_;
}
void interface_free(Interface *interface){
    Interface_ *interface_ = (Interface_ *) interface;
    if(interface_ == NULL){
        return;
    }
    protocol_free(interface_->protocol);
    stream_free(interface_->stream);
    caster_free(interface_->caster);
    kmap_free(interface_->self);
    kmap_free(interface_->items);
    kmemory_free(interface_->interface_path);
    kmemory_free(interface_->self_id);
    kmemory_free(interface_);
}