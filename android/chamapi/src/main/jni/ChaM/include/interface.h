#ifndef CHAM_INTERFACE_H
#define CHAM_INTERFACE_H

#include "dirent.h"
#include "sys/stat.h"
#include "sqlite3.h"
#include "protocol.h"
#include "stream.h"
#include "caster.h"

typedef enum InterfaceState{
    INTERFACE_STATE_STOP,     // not connected to server and not start
    INTERFACE_STATE_START,    // started but not connected to server
    INTERFACE_STATE_OFFLINE,  // connected to server but
    INTERFACE_STATE_ONLINE    // connected to server and login
} InterfaceState;
typedef enum InterfaceType{
    INTERFACE_APPRESTART,
    INTERFACE_APPUPGRADE,
    INTERFACE_APPHANDSHAKE,
    INTERFACE_APPONLINE,
    INTERFACE_APPOFFLINE,
    INTERFACE_STREAMSET,
    INTERFACE_STREAMGET
} InterfaceType;
typedef struct Interface{
    int (*start)(struct Interface *interface);
    InterfaceState (*state)(struct Interface *interface);
    int (*stop)(struct Interface *interface);
    int (*self_set)(struct Interface *interface , char *key , char *value);
    char* (*self_get)(struct Interface *interface , char *key);

    Kmap* (*protocol_request)(struct Interface *interface , ProtocolType protocol_type , ...);
    int (*protocol_update)(struct Interface *interface , void *data , char *token , void (*destructor)(void *old_data));
    int (*protocol_peer_captcha)(struct Interface *interface , void *data , char *token , char *phone , char *type);
    int (*protocol_peer_login)(struct Interface *interface , void *data , char *token , char *id , char *scode , char *platform , char *version);
    int (*protocol_peer_logout)(struct Interface *interface , void *data , char *token);
    int (*protocol_peer_create)(struct Interface *interface , void *data , char *token , char *phone , char *captcha);
    int (*protocol_peer_delete)(struct Interface *interface , void *data , char *token);
    int (*protocol_peer_load)(struct Interface *interface , void *data , char *token , char *id , char *type , char *mode , char *param_1 , char *param_2 , int local , int try);
    int (*protocol_peer_set)(struct Interface *interface , void *data , char *token , char *id , char *key , char *option , char *value , char *operator);
    int (*protocol_peer_id)(struct Interface *interface , void *data , char *token , char *key , char *type);
    int (*protocol_connection_create)(struct Interface *interface , void *data , char *token);
    int (*protocol_connection_delete)(struct Interface *interface , void *data , char *token , char *id);
    int (*protocol_connection_set)(struct Interface *interface , void *data , char *token , char *id , char *key , char *option , char *value , char *operator);
    int (*protocol_connection_id)(struct Interface *interface , void *data , char *token , char *key , char *type);
    int (*protocol_message_set)(struct Interface *interface , void *data , char *token , char *conection_id , char *message_type , char *forward_id , char *reply_date , char *data_1 , char *data_2);
    int (*protocol_message_handle)(struct Interface *interface , void *data , char *token , char *conection_id , char *handle);

    int (*stream_update)(struct Interface *interface , void *data , char *link , void (*destructor)(void *old_data));
    int (*stream_set)(struct Interface *interface , void *data , char *link , char *path , int calltime);
    int (*stream_get)(struct Interface *interface , void *data , char *link , char *path , int calltime);
    Kmap* (*stream_sets)(struct Interface *interface);
    Kmap* (*stream_gets)(struct Interface *interface);
    int (*stream_cancel)(struct Interface *interface , char *link);
    Kmap* (*stream_state)(struct Interface *interface , char *link);
    int (*stream_clear)(struct Interface *interface);
    long (*stream_size)(struct Interface *interface);
    char* (*stream_getdata)(struct Interface *interface , char *link);
    int (*stream_setdata)(struct Interface *interface , char *link , char *state);
    char* (*stream_file)(struct Interface *interface , char *link , int recreate);
    char* (*stream_temp)(struct Interface *interface , int recreate);
    char* (*stream_encode)(struct Interface *interface , char *name , char *size , char *mime , char *checksum);
    Kmap* (*stream_decode)(struct Interface *interface , char *link);

    int (*caster_tune)(struct Interface *interface , char *target_id , CasterTune tune);
    int (*caster_cast)(struct Interface *interface , void *data , int size , int end);
    void **bundles;
} Interface;

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
);

void interface_free(Interface *interface);

#endif //CHAM_INTERFACE_H
