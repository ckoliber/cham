#ifndef KLB_KARRAY_H
#define KLB_KARRAY_H

/// Value -> should be address !!!

#include <string.h>
#include <stdlib.h>

typedef enum KarrayRepetitionType{
    KARRAY_REPETITION_SET,
    KARRAY_REPETITION_LIST
} KarrayRepetitionType;

typedef struct Karray{
    KarrayRepetitionType repetition_type;
    void **data_address;
    int data_length;
    int (*equality_function)(void*,void*);
    void (*destructor_function)(void*);
} Karray;

Karray *karray_new(KarrayRepetitionType repetition_type , int (*equality_function)(void*,void*) , void (*destructor_function)(void*));

Karray *karray_add(Karray *karray , void *data);

Karray *karray_put(Karray *karray , int index , void *data);

Karray *karray_remove(Karray *karray , int index);

void *karray_get(Karray *karray , int index);

int karray_indexof(Karray *karray , void *data);

int karray_length(Karray *karray);

void karray_free(Karray *karray);

#endif //KLB_KARRAY_H
