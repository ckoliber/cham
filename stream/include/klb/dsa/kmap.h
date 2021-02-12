#ifndef KLB_KMAP_H
#define KLB_KMAP_H

#include <string.h>
#include <stdlib.h>

typedef struct Kmap{
    void **key_address;
    void **value_address;
    int key_length;
    int (*key_equality_function)(void*,void*);
    void (*key_destructor_function)(void*);
    void (*value_destructor_function)(void*);
} Kmap;

Kmap *kmap_new(int (*key_equality_function)(void*,void*),void (*key_destructor_function)(void*),void (*value_destructor_function)(void*));

Kmap *kmap_refresh(Kmap *kmap , void *key , void *value);

Kmap *kmap_put(Kmap *kmap , void *key , void *value);

Kmap *kmap_remove(Kmap *kmap , void *key);

void *kmap_get(Kmap *kmap , void *key);

void *kmap_key(Kmap *kmap , int index);

void *kmap_value(Kmap *kmap , int index);

int kmap_indexof(Kmap *kmap , void *key);

int kmap_length(Kmap *kmap);

void kmap_free(Kmap *kmap);

#endif //KLB_KMAP_H
