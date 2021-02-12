#ifndef KLB_KUDP_H
#define KLB_KUDP_H

#include "arpa/inet.h"
#include "sys/socket.h"
#include "netdb.h"
#include "netinet/in.h"
#include "stdlib.h"
#include "kprocessor.h"

#define PORT_RANDOM 0

typedef struct KudpData{
    void *data;
    int data_size;
} KudpData;

typedef struct KudpPeer{
    int socket_fd;
    char *peer_host;
    int peer_port;
} KudpPeer;

typedef struct Kudp{
    int buffer_size;
    int timeout;
    int socket_fd;
    Kprocessor *processor;
    int listen_port;
    void (*onSpawn)();
    void (*onData)(KudpPeer *peer , KudpData *data);
} Kudp;

Kudp *kudp_new(int buffer_size , int timeout);

Kudp *kudp_bind(
        Kudp *kudp,
        int pool_size,
        int listen_port,
        void (*onSpawn)(),
        void (*onData)(KudpPeer *peer , KudpData *data)
);

void kudp_sendto(KudpPeer *peer , KudpData *data);

KudpPeer *kudp_peer(Kudp *kudp , char *host , int port);

Kudp *kudp_close(Kudp *kudp);

void kudp_free(Kudp *kudp);

#endif //KLB_KUDP_H
