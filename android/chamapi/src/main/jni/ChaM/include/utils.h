#ifndef CHAM_CHAM_H
#define CHAM_CHAM_H

#if defined(__unix__) && !defined(__ANDROID__)
#include <signal.h>
#include <mqueue.h>
#include <sys/epoll.h>
#endif
#if defined(__ANDROID__)
#include <unistd.h>
#endif
#include <time.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <semaphore.h>
#include <fcntl.h>
#include <zconf.h>
#include <sys/time.h>
#include <pthread.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <poll.h>
#define MAX_EPOLL_EVENTS    1024
#define MAX_POLL_SET 1024

typedef enum Kconcurrency{
    KCONCURRENCY_SYNCRONIZED,   // read , write in sync             ( if write -> no read , no write ) ( if read -> no read , no write )
    KCONCURRENCY_CONCURRENT,    // read in parallel , write in sync ( if write -> no read , no write ) ( if read -> read , no write )
    KCONCURRENCY_PARALLEL       // read , write in parallel         ( if write -> read , write ) ( if read -> read , write )
} Kconcurrency;
typedef enum Ktype{
    KTYPE_STACK,
    KTYPE_HEAP
} Ktype;
typedef struct Kdata{
    void *data;
    Ktype type;
} Kdata;

char *kmemory_print_token(char *format);
char *kmemory_print_string(char *format,...);
void *kmemory_copy_struct(void *data,int size);
char *kmemory_copy_string(char *data);
char *kmemory_copy_integer(int data);
char *kmemory_copy_long(long data);
void *kmemory_alloc(size_t byte_count);
void kmemory_free(void *address);


typedef enum KsemMode{
    KSEM_MODE_CREATE,
    KSEM_MODE_OPEN
} KsemMode;
typedef struct Ksem{
    int (*value)(struct Ksem *ksem);
    int (*post)(struct Ksem *ksem);
    int (*wait)(struct Ksem *ksem);
    int (*destroy)(struct Ksem *ksem);
} Ksem;
Ksem *ksem_new(KsemMode mode , char *name , int value);
void ksem_free(Ksem *ksem);


typedef enum KmsgMode{
    KMSG_MODE_CREATE,
    KMSG_MODE_OPEN,
    KMSG_MODE_PRIVATE
} KmsgMode;
typedef struct Kmsg{
    int (*refresh)(struct Kmsg *kmsg);
    int (*isempty)(struct Kmsg *kmsg);
    int (*push)(struct Kmsg *kmsg , void *data);
    void* (*poll)(struct Kmsg *kmsg);
    int (*destroy)(struct Kmsg *kmsg);
} Kmsg;
Kmsg *kmsg_new(KmsgMode mode , int msg_size , int msg_count , char *name);
void kmsg_free(Kmsg *kmsg);


typedef struct Karray{
    int (*add)(struct Karray *karray,void *data,Ktype data_type);
    int (*put)(struct Karray *karray,int index,void *data,Ktype data_type);
    int (*remove)(struct Karray *karray,int index);
    void* (*get)(struct Karray *karray,int index);
    int (*indexof)(struct Karray *karray,void *data);
    int (*length)(struct Karray *karray);
} Karray;
Karray *karray_new(Kconcurrency kconcurrency , float growth_factor , int (*comparator)(void *data_1,void *data_2) , void (*destructor)(void *data));
void karray_free(Karray *karray);


typedef struct Klist{
    int (*add)(struct Klist *klist,void *data,Ktype data_type);
    int (*put)(struct Klist *klist,int index,void *data,Ktype data_type);
    int (*remove)(struct Klist *klist,int index);
    void* (*get)(struct Klist *klist,int index);
    int (*indexof)(struct Klist *klist,void *data);
    int (*length)(struct Klist *klist);
} Klist;
Klist *klist_new(Kconcurrency kconcurrency , int (*comparator)(void *data_1,void *data_2) , void (*destructor)(void *data));
void klist_free(Klist *klist);


typedef struct Kmap{
    void* (*getkey)(struct Kmap *kmap,int index);
    void* (*getvalue)(struct Kmap *kmap,int index);
    int (*put)(struct Kmap *kmap,void *key,Ktype key_type,void *value,Ktype value_type);
    int (*remove)(struct Kmap *kmap,void *key);
    void* (*get)(struct Kmap *kmap,void *key);
    int (*indexof)(struct Kmap *kmap,void *key);
    int (*length)(struct Kmap *kmap);
} Kmap;
Kmap *kmap_new(Kconcurrency kconcurrency , float growth_factor , int (*key_comparator)(void *key_1,void *key_2),void (*key_destructor)(void *key),void (*value_destructor)(void *value));
void kmap_free(Kmap *kmap);


Kmap *kson_parse(char *kson);
char *kson_pack(Kmap *kmap);
Karray *kson_array_parse(char *kson_array);
char *kson_array_pack(Karray *karray);


typedef enum KprocessorType{
    KPROCESSOR_TYPE_THREAD,
    KPROCESSOR_TYPE_PROCESS
} KprocessorType;
typedef struct Kprocessor {
    int (*start)(struct Kprocessor *kprocessor);
    long (*post)(struct Kprocessor *kprocessor , void* (*function)(void*) , void *arg);
    int (*stop)(struct Kprocessor *kprocessor);
} Kprocessor;
Kprocessor *kprocessor_new(KprocessorType type , int size , char *name , void (*handler)(long token , void *result));
void kprocessor_free(Kprocessor *kprocessor);


typedef enum KtcpTune{
    KTCP_TUNE_RDWR,
    KTCP_TUNE_RDONLY,
    KTCP_TUNE_WRONLY,
    KTCP_TUNE_NONE,
} KtcpTune;
typedef struct KtcpPeer{
    int fd;
    int index;
} KtcpPeer;
typedef struct Ktcp{
    int (*listen)(struct Ktcp *ktcp , char *name , int max_connections , int listen_port);
    int (*connectivity)(struct Ktcp *ktcp , char *name);
    int (*connect)(struct Ktcp *ktcp , void *arg , char *host , int port);
    int (*tune)(struct Ktcp *ktcp , KtcpPeer *kpeer , KtcpTune ktune);
    char* (*gethost)(struct Ktcp *ktcp , KtcpPeer *kpeer);
    int (*getport)(struct Ktcp *ktcp , KtcpPeer *kpeer);
    int (*send)(struct Ktcp *ktcp , KtcpPeer *kpeer , char *data , int size);
    int (*close)(struct Ktcp *ktcp , KtcpPeer *kpeer);
    int (*shutdown)(struct Ktcp *ktcp);
    void *bundle;
} Ktcp;
Ktcp *ktcp_new(
        int pool_size,
        int buffer_size ,
        void (*onSpawn)(Ktcp *ktcp) ,
        void (*onOpen)(Ktcp *ktcp , KtcpPeer *kpeer , void *arg) ,
        void (*onReadable)(Ktcp *ktcp , KtcpPeer *kpeer , char *data , int size) ,
        void (*onWritable)(Ktcp *ktcp , KtcpPeer *kpeer) ,
        void (*onClose)(Ktcp *ktcp , KtcpPeer *kpeer)
);
void ktcp_free(Ktcp *ktcp);


typedef struct KudpPeer{
    int fd;
    int index;
    char *host;
    int port;
} KudpPeer;
typedef struct Kudp{
    int (*listen)(struct Kudp *kudp , char *name , int listen_port);
    int (*binder)(struct Kudp *kudp , char *name);
    int (*bind)(struct Kudp *kudp , void *arg , char *host , int port);
    int (*sendto)(struct Kudp *kudp , KudpPeer *kpeer , char *data , int size , int end);
    int (*close)(struct Kudp *kudp , KudpPeer *kpeer);
    int (*shutdown)(struct Kudp *kudp);
    void *bundle;
} Kudp;
Kudp *kudp_new(
        int pool_size,
        int buffer_size,
        void (*onSpawn)(struct Kudp *kudp),
        void (*onOpen)(struct Kudp *kudp , KudpPeer *kpeer , void *arg),
        void (*onData)(struct Kudp *kudp , KudpPeer *kpeer , char *data , int size),
        void (*onClose)(struct Kudp *kudp , KudpPeer *kpeer)
);
void kudp_free(Kudp *kudp);

#endif //CHAM_CHAM_H
