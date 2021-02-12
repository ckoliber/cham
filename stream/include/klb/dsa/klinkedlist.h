#ifndef KLB_KLINKED_H
#define KLB_KLINKED_H

/// Value -> should be address !!!
/// Shared Key ->
///     -1  -> Private  -> malloc(...)
///     *   -> Shared   -> shmget(...)

#include <string.h>
#include <stdlib.h>
#include "sys/shm.h"

typedef struct Klinked{
    void *data;
    struct Klinked *next;
} Klinked;

typedef struct Klinkedlist{
    Klinked *head;
    int shared_id;
    int length;
    int (*equality_function)(void*,void*);
    void (*destructor_function)(void*);
} Klinkedlist;

Klinkedlist *klinkedlist_new(int shared_key , int (*equality_function)(void*,void*) , void (*destructor_function)(void*));

Klinkedlist *klinkedlist_add(Klinkedlist *klinkedlist , void *data);

Klinkedlist *klinkedlist_put(Klinkedlist *klinkedlist , int index , void *data);

Klinkedlist *klinkedlist_remove(Klinkedlist *klinkedlist , int index);

void *klinkedlist_get(Klinkedlist *klinkedlist , int index);

int klinkedlist_indexof(Klinkedlist *klinkedlist , void *data);

int klinkedlist_length(Klinkedlist *klinkedlist);

void klinkedlist_free(Klinkedlist *klinkedlist);

#endif //KLB_KLINKED_H
