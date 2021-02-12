#include "utils.h"
#include "stdio.h"

typedef enum ProtocolState{
    PROTOCOL_STATE_STOP,     // not connected to server and not start
    PROTOCOL_STATE_START,    // started but not connected to server
    PROTOCOL_STATE_OFFLINE,  // connected to server but
    PROTOCOL_STATE_ONLINE    // connected to server and login
} ProtocolState;
typedef enum ProtocolType{
    PROTOCOL_PEER_CAPTCHA,
    PROTOCOL_PEER_LOGIN,
    PROTOCOL_PEER_LOGOUT,
    PROTOCOL_PEER_CREATE,
    PROTOCOL_PEER_DELETE,
    PROTOCOL_PEER_LOAD,
    PROTOCOL_PEER_SET,
    PROTOCOL_PEER_ID,
    PROTOCOL_CONNECTION_CREATE,
    PROTOCOL_CONNECTION_DELETE,
    PROTOCOL_CONNECTION_SET,
    PROTOCOL_CONNECTION_ID,
    PROTOCOL_MESSAGE_SET,
    PROTOCOL_MESSAGE_HANDLE
} ProtocolType;
typedef struct Protocol {
    int (*start)(struct Protocol *protocol);
    ProtocolState (*state)(struct Protocol *protocol);
    int (*stop)(struct Protocol *protocol);
    Kmap* (*request)(struct Protocol *protocol , ProtocolType protocol_type , va_list variable_list);
    int (*request_size)(struct Protocol *protocol , ProtocolType protocol_type);
    Kmap* (*peer_captcha)(struct Protocol *protocol , char *token , char *phone , char *type);
    Kmap* (*peer_login)(struct Protocol *protocol , char *token , char *id , char *scode , char *platform , char *version);
    Kmap* (*peer_logout)(struct Protocol *protocol , char *token);
    Kmap* (*peer_create)(struct Protocol *protocol , char *token , char *phone , char *captcha);
    Kmap* (*peer_delete)(struct Protocol *protocol , char *token);
    Kmap* (*peer_load)(struct Protocol *protocol , char *token , char *id , char *type , char *mode , char *param_1 , char *param_2);
    Kmap* (*peer_set)(struct Protocol *protocol , char *token , char *id , char *key , char *option , char *value , char *operator);
    Kmap* (*peer_id)(struct Protocol *protocol , char *token , char *key , char *type);
    Kmap* (*connection_create)(struct Protocol *protocol , char *token);
    Kmap* (*connection_delete)(struct Protocol *protocol , char *token , char *id);
    Kmap* (*connection_set)(struct Protocol *protocol , char *token , char *id , char *key , char *option , char *value , char *operator);
    Kmap* (*connection_id)(struct Protocol *protocol , char *token , char *key , char *type);
    Kmap* (*message_set)(struct Protocol *protocol , char *token , char *conection_id , char *message_type , char *forward_id , char *reply_date , char *data_1 , char *data_2);
    Kmap* (*message_handle)(struct Protocol *protocol , char *token , char *conection_id , char *handle);
    void *bundle;
} Protocol;

Protocol *protocol_new(
        char *protocol_host,
        int protocol_port,
        void (*onRequest)(struct Protocol *protocol , ProtocolType type , char *token , Kmap *request),
        void (*onResponse)(struct Protocol *protocol , ProtocolType type , char *token , Kmap *response)
);
void protocol_free(Protocol *protocol);
