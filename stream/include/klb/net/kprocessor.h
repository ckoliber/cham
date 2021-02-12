#ifndef KLB_KPROCESSOR_H
#define KLB_KPROCESSOR_H

#define MAX_POOL_SIZE 64

#include "memory.h"
#include "sys/time.h"
#include <signal.h>
#include "unistd.h"
#include "stdlib.h"
#include "pthread.h"
#include "sys/msg.h"
#include "sys/shm.h"

typedef enum KprocessorPoolType{
    KPROCESSOR_POOL_TYPE_THREAD,
    KPROCESSOR_POOL_TYPE_PROCESS
} KprocessorPoolType;

typedef struct Kprocessor {
    int shared_id;
    void (*handler)(long token , void *result);
    KprocessorPoolType pool_type;
    int pool_size;
    int pool_queue;
    int pool_list[MAX_POOL_SIZE];
} Kprocessor;

Kprocessor *kprocessor_new(int shared_key , KprocessorPoolType pool_type , int pool_size);

Kprocessor *kprocessor_start(Kprocessor *kprocessor , void (*handler)(long token , void *result));

long kprocessor_post(Kprocessor *kprocessor , void* (*function)(void*) , void *arg);

void kprocessor_stop(Kprocessor *kprocessor);

void kprocessor_free(Kprocessor *kprocessor);

#endif //KLB_KPROCESSOR_H
