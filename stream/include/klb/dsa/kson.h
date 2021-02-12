#ifndef KLB_KSON_H
#define KLB_KSON_H

#include "kmap.h"
#include "karray.h"

int kson_equality(void *data1 , void *data2);

void kson_destructor(void *data);

Kmap *kson_parse(char *kson);   // free map

char *kson_pack(Kmap *kmap);    // free char*

Karray *kson_array_parse(char *kson_array);  // free list

char *kson_array_pack(Karray *karray);         // free char*

#endif //KLB_KSON_H
