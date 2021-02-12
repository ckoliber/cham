#include "utils.h"

// any input arg of char* is freeable -> even kmsg void* -> copy to struct

// %li (seconds) , %li (nano seconds)
char *kmemory_print_token(char *format){
    struct timespec time;
    clock_gettime(CLOCK_MONOTONIC,&time);
    char *result = kmemory_alloc(16384);
    memset(result,0,16384);
    sprintf(result,format,time.tv_sec,time.tv_nsec);
    result = realloc(result , strlen(result)+1);
    return result;
}
char *kmemory_print_string(char *format , ...){
    char *result = kmemory_alloc(16384);
    memset(result,0,16384);
    va_list variable_list;
    va_start(variable_list , format);
    vsprintf(result,format,variable_list);
    va_end(variable_list);
    result = realloc(result , strlen(result)+1);
    return result;
}
void *kmemory_copy_struct(void *data,int size){
    void *data_struct = kmemory_alloc((size_t) size);
    memset(data_struct, 0, (size_t) size);
    memcpy(data_struct, data, (size_t) size);
    return data_struct;
}
char *kmemory_copy_string(char *data){
    char *data_text = kmemory_alloc(strlen(data)+1);
    memset(data_text,0,strlen(data)+1);
    sprintf(data_text,"%s",data);
    data_text = realloc(data_text,strlen(data_text)+1);
    return data_text;
}
char *kmemory_copy_integer(int data){
    char *data_text = kmemory_alloc(1024);
    memset(data_text,0,1024);
    sprintf(data_text,"%d",data);
    data_text = realloc(data_text,strlen(data_text)+1);
    return data_text;
}
char *kmemory_copy_long(long data){
    char *data_text = kmemory_alloc(1024);
    memset(data_text,0,1024);
    sprintf(data_text,"%li",data);
    data_text = realloc(data_text,strlen(data_text)+1);
    return data_text;
}
void *kmemory_alloc(size_t byte_count){
    return malloc(byte_count);
}
void kmemory_free(void *address){
    free(address);
}