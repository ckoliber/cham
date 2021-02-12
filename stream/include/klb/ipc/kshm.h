#ifndef KLB_KSHM_H
#define KLB_KSHM_H

/// Simple and powerful implementation of System V Shared Memories !

#include "memory.h"
#include "sys/shm.h"
#include "stdlib.h"
#include "ksem.h"

typedef enum KshmMode{
    KSHM_MODE_CHECK,    // check    -> create   ->      only check exists and connect
    KSHM_MODE_CREATE,   // create   -> create   ->      create if not exits and connect
    KSHM_MODE_DESTROY,  // destroy  -> free     ->      destroy if exits
    KSHM_MODE_NONE      // none     -> free     ->      only clear space
} KshmMode;

typedef enum KshmKey{
    KSHM_KEY_PATH,
    KSHM_KEY_RANDOM,
    KSHM_KEY_CUSTOM
} KshmKey;

typedef enum KshmConcurrent{
    KSHM_CONCURRENT_FULL,   // when writing     -> no reading   ,   no writing
    KSHM_CONCURRENT_HALF,   // when writing     -> reading      ,   no writing
    KSHM_CONCURRENT_NONE    // when writing     -> reading      ,   writing
} KshmConcurrent;

typedef struct Kshm{
    int id;
    void *memory_address;
    KshmConcurrent  memory_concurrent;
    Ksem *memory_semaphore;
} Kshm;

Kshm *kshm_new(KshmKey kshm_key , char *key_generator , KshmConcurrent memory_concurrent , size_t memory_size, KshmMode mode);

void kshm_write(Kshm *kshm , int offset , void *data , int data_size);

void kshm_set(Kshm *kshm , int offset , int length , int data);

void* kshm_read(Kshm *kshm , int offset , int length);

void kshm_free(Kshm *kshm , KshmMode mode);

#endif //KLB_KSHM_H
