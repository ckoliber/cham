#ifndef KLB_KSEM_H
#define KLB_KSEM_H

/// Simple and powerful implementation of System V Semaphores !

#include "memory.h"
#include "sys/sem.h"
#include "stdlib.h"

typedef enum KsemMode{
    KSEM_MODE_CHECK,    // check    -> create   ->      only check exists and connect
    KSEM_MODE_CREATE,   // create   -> create   ->      create if not exits and connect
    KSEM_MODE_DESTROY,  // destroy  -> free     ->      destroy if exits
    KSEM_MODE_NONE      // none     -> free     ->      only clear space
} KsemMode;

typedef enum KsemKey{
    KSEM_KEY_PATH,
    KSEM_KEY_RANDOM,
    KSEM_KEY_CUSTOM
} KsemKey;

typedef enum KsemBlock{
    KSEM_BLOCK_BLOCKING,
    KSEM_BLOCK_NONBLOCKING
} KsemBlock;

typedef struct Ksem{
    int id;
} Ksem;

Ksem *ksem_new(KsemKey ksem_key , char *key_generator , int sem_count , int sem_defaults[] , KsemMode mode);

int ksem_get(Ksem *ksem , int sem_num);

void ksem_wait(Ksem *ksem , int sem_num , int min_value);

int ksem_set(Ksem *ksem , KsemBlock sem_block , int sem_num , int value);

void ksem_free(Ksem *ksem , KsemMode mode);

#endif //KLB_KSEM_H
