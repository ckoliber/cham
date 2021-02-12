#ifndef KLB_KTCP_H
#define KLB_KTCP_H

#define MAX_EPOLL_EVENTS    1024

#include "sys/epoll.h"
#include "sys/socket.h"
#include "sys/types.h"
#include "fcntl.h"
#include "arpa/inet.h"
#include "signal.h"
#include "netdb.h"
#include "netinet/in.h"
#include "stdlib.h"
#include "poll.h"
#include "limits.h"
#include "kprocessor.h"

#define MAX_POLL_SET 1024
#define PORT_RANDOM 0

// data is not need to remove

typedef enum KtcpTimeout{
    KTCP_TIMEOUT_RECV,
    KTCP_TIMEOUT_SEND
} KtcpTimeout;

typedef enum KtcpTune{
    KTCP_TUNE_RDWR,
    KTCP_TUNE_RDONLY,
    KTCP_TUNE_WRONLY,
    KTCP_TUNE_NONE,
} KtcpTune;

typedef struct KtcpData{
    void *data;
    int data_size;
} KtcpData;

typedef struct KtcpPeer{
    int socket_fd;
    int poll_index;
    struct Ktcp *ktcp;
} KtcpPeer;

typedef struct Ktcp{
    int pool_size;
    struct pollfd **poll_set;
    int epoll_fd;
    int buffer_size;
    Kprocessor *processor;
    int max_connections;
    int listen_port;
    void (*onSpawn)();
    void (*onOpen)(KtcpPeer *peer);
    void (*onReadable)(KtcpPeer *peer , KtcpData *data);
    void (*onWritable)(KtcpPeer *peer);
    void (*onClose)(KtcpPeer *peer);
} Ktcp;

Ktcp *ktcp_new(int buffer_size);

Ktcp *ktcp_listen(
        Ktcp *ktcp,
        int pool_size,
        int max_connections,
        int listen_port,
        void (*onSpawn)(),
        void (*onOpen)(KtcpPeer *peer),
        void (*onReadable)(KtcpPeer *peer , KtcpData *data),
        void (*onWritable)(KtcpPeer *peer),
        void (*onClose)(KtcpPeer *peer)
);

Ktcp *ktcp_connectivity(
        Ktcp *ktcp,
        int pool_size,
        void (*onSpawn)() ,
        void (*onOpen)(KtcpPeer *peer),
        void (*onReadable)(KtcpPeer *peer , KtcpData *data),
        void (*onWritable)(KtcpPeer *peer),
        void (*onClose)(KtcpPeer *peer)
);

int ktcp_connect(Ktcp *ktcp, int target_port, char *target_host);

void ktcp_tune(KtcpPeer *peer , KtcpTune ktcp_tune);

void ktcp_timeout(KtcpPeer *peer , KtcpTimeout timeout , int time_second);

void ktcp_send(KtcpPeer *peer , KtcpData *data);

void ktcp_shutdown(KtcpPeer *peer);

Ktcp *ktcp_close(Ktcp *ktcp);

void ktcp_free(Ktcp *ktcp);

#endif //KLB_KTCP_H
