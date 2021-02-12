#ifndef KLB_KMSG_H
#define KLB_KMSG_H

/// Simple and powerful implementation of System V Message Queues !

#include "memory.h"
#include "sys/msg.h"
#include "stdlib.h"

typedef enum KmsgMode{
    KMSG_MODE_CHECK,    // check    -> create   ->      only check exists and connect
    KMSG_MODE_CREATE,   // create   -> create   ->      create if not exits and connect
    KMSG_MODE_DESTROY,  // destroy  -> free     ->      destroy if exits
    KMSG_MODE_NONE      // none     -> free     ->      only clear space
} KmsgMode;

typedef enum KmsgKey{
    KMSG_KEY_PATH,
    KMSG_KEY_RANDOM,
    KMSG_KEY_CUSTOM
} KmsgKey;

typedef enum KmsgBlock{
    KMSG_BLOCK_BLOCKING,
    KMSG_BLOCK_NONBLOCKING
} KmsgBlock;

typedef struct Kmsg{
    int id;
    size_t message_size;
} Kmsg;

Kmsg *kmsg_new(KmsgKey kmsg_key , char *key_generator , size_t message_size , KmsgMode mode);

void* kmsg_recv(Kmsg *kmsg , KmsgBlock message_block); // free void* at end

void kmsg_send(Kmsg *kmsg , void *message);

void kmsg_free(Kmsg *kmsg , KmsgMode mode);

#endif //KLB_KMSG_H
