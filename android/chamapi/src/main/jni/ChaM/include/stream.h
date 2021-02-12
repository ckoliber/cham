#include <utils.h>
//#include "openssl/rc4.h"

typedef enum StreamType{
    STREAM_START,
    STREAM_OPEN,
    STREAM_HANDSHAKE,
    STREAM_DATA,
    STREAM_STOP
} StreamType;
typedef enum StreamParallelism{
    STREAM_PARALLELISM_PARALLEL,
    STREAM_PARALLELISM_SEQUENTIAL
} StreamParallelism;
typedef enum StreamPipeType{
    STREAM_PIPE_TYPE_SET,
    STREAM_PIPE_TYPE_GET
} StreamPipeType;
typedef enum StreamPipeState{
    STREAM_PIPE_STATE_START,
    STREAM_PIPE_STATE_OPEN,
    STREAM_PIPE_STATE_HANDSHAKE
} StreamPipeState;
typedef struct StreamPipe{
    StreamPipeType type;
    StreamPipeState state;
    char *link;
    char *path;
    KtcpPeer *kpeer;
    long size;
    long seek;
    int fd;
    int calltime;
    int callback;
} StreamPipe;
typedef struct Stream{
    int (*start)(struct Stream *stream , char *setting_id , char *setting_scode);
    int (*set)(struct Stream *stream , char *link , char *path , int calltime);
    int (*get)(struct Stream *stream , char *link , char *path , int calltime);
    Kmap* (*sets)(struct Stream *stream);
    Kmap* (*gets)(struct Stream *stream);
    int (*cancel)(struct Stream *stream , char *link);
    StreamPipe* (*state)(struct Stream *stream , char *link);
    int (*stop)(struct Stream *stream);
    char* (*encode)(struct Stream *stream , Kmap *kmap);
    Kmap* (*decode)(struct Stream *stream , char *cipher);
    void *bundle;
} Stream;

Stream *stream_new(
        StreamParallelism parallelism,
        int buffer_size,
        int pool_size,
        char *stream_host,
        int stream_port,
        void (*onStart)(struct Stream *stream , char *link),
        void (*onOpen)(struct Stream *stream , char *link),
        void (*onHandshake)(struct Stream *stream , char *link),
        void (*onData)(struct Stream *stream , char *link , long seek),
        void (*onStop)(struct Stream *stream , char *link)
);

void stream_free(Stream *stream);