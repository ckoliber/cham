#include "utils.h"

char *kson_string_parse(char *string);
char *kson_string_pack(char *string);
int kson_comparator(void *data1 , void *data2);

char *kson_string_parse(char *string){
    if(string != NULL){
        int string_length = (int) strlen(string);
        char *buffer = kmemory_alloc(sizeof(char)*string_length*2);
        memset(buffer , 0 , sizeof(char)*string_length*2);
        int str_cursor = 0;
        for(int cursor = 0 ; cursor < string_length ; cursor++){
            if(string[cursor] == '\\' && string[cursor+1] == '\"'){
                buffer[str_cursor] = '\"';
                cursor++;
            }else{
                buffer[str_cursor] = string[cursor];
            }
            str_cursor++;
        }
        buffer[str_cursor] = '\0';
        buffer = realloc(buffer , strlen(buffer)+1);
        return buffer;
    }else{
        return "";
    }
}
char *kson_string_pack(char *string){
    if(string != NULL){
        int string_length = (int) strlen(string);
        char *buffer = kmemory_alloc(sizeof(char)*string_length*2);
        memset(buffer , 0 , sizeof(char)*string_length*2);
        int str_cursor = 0;
        for(int cursor = 0 ; cursor < string_length ; cursor++){
            if(string[cursor] == '\"'){
                buffer[str_cursor] = '\\';
                str_cursor++;
                buffer[str_cursor] = '\"';
            }else{
                buffer[str_cursor] = string[cursor];
            }
            str_cursor++;
        }
        buffer[str_cursor] = '\0';
        return buffer;
    }else{
        return "";
    }
}
int kson_comparator(void *data1 , void *data2){
    return strcmp(data1 , data2) == 0;
}
Kmap *kson_parse(char *kson){
    int kson_length = (int) strlen(kson);
    Kmap *kmap = kmap_new(KCONCURRENCY_PARALLEL , 2.0f , kson_comparator , NULL , NULL);
    char *key = NULL;
    char *value = NULL;
    int i_cursor = 0;
    int is_inner_key = 0;
    int is_inner_value = 0;
    for(int cursor = 0 ; cursor < kson_length ; cursor++){
        if(is_inner_key == 0){
            if(kson[cursor] == '\"' && kson[cursor-1] != '\\'){
                is_inner_key = 1;
                key = kmemory_alloc(sizeof(char)*kson_length);
                memset(key , 0 , sizeof(char)*kson_length);
            }
        }else if(is_inner_key == 1){
            if(kson[cursor] == '\"' && kson[cursor-1] != '\\'){
                is_inner_key = 2;
                key[i_cursor] = '\0';
                i_cursor = 0;
            }else{
                key[i_cursor] = kson[cursor];
                i_cursor++;
            }
        }else{
            if(!is_inner_value){
                if(kson[cursor] == '\"' && kson[cursor-1] != '\\'){
                    is_inner_value = 1;
                    value = kmemory_alloc(sizeof(char)*kson_length);
                    memset(value , 0 , sizeof(char)*kson_length);
                }
            }else{
                if(kson[cursor] == '\"' && kson[cursor-1] != '\\'){
                    value[i_cursor] = '\0';
                    is_inner_value = 0;
                    is_inner_key = 0;
                    i_cursor = 0;
                    char *key_string = kson_string_parse(key);
                    kmemory_free(key);
                    char *value_string = kson_string_parse(value);
                    kmemory_free(value);
                    kmap->put(kmap,key_string,KTYPE_HEAP,value_string,KTYPE_HEAP);
                }else{
                    value[i_cursor] = kson[cursor];
                    i_cursor++;
                }
            }
        }
    }
    return kmap;
}
char *kson_pack(Kmap *kmap){
    int kmap_length = 0;
    for(int a = 0 ; a < kmap->length(kmap) ; a++){
        kmap_length += (strlen(kmap->getkey(kmap,a)) + strlen(kmap->getvalue(kmap,a)));
    }
    char *pack = kmemory_alloc(sizeof(char)*kmap_length*2+2+kmap->length(kmap)*6);
    memset(pack , 0 , sizeof(char)*kmap_length*2+2+kmap->length(kmap)*6);
    strcat(pack,"{");
    for(int cursor = 0 ; cursor < kmap->length(kmap) ; cursor++){
        char *key = kson_string_pack(kmap->getkey(kmap , cursor));
        char *value = kson_string_pack(kmap->getvalue(kmap , cursor));
        strcat(pack,"\"");
        strcat(pack,key);
        strcat(pack,"\":\"");
        strcat(pack,value);
        strcat(pack,"\"");
        if(cursor < kmap->length(kmap) - 1){
            strcat(pack,",");
        }
        kmemory_free(key);
        kmemory_free(value);
    }
    strcat(pack,"}");
    return pack;
}
Karray *kson_array_parse(char *kson_array){
    int kson_array_length = (int) strlen(kson_array);
    Karray *karray = karray_new(KCONCURRENCY_PARALLEL,2.0f,kson_comparator,NULL);
    char *value = NULL;
    int i_cursor = 0;
    int is_inner_value = 0;
    for(int cursor = 0 ; cursor < kson_array_length ; cursor++){
        if(is_inner_value == 0){
            if(kson_array[cursor] == '\"' && kson_array[cursor-1] != '\\'){
                is_inner_value = 1;
                value = kmemory_alloc(sizeof(char)*kson_array_length);
                memset(value , 0 , sizeof(char)*kson_array_length);
            }
        }else{
            if(kson_array[cursor] == '\"' && kson_array[cursor-1] != '\\'){
                value[i_cursor] = '\0';
                is_inner_value = 0;
                i_cursor = 0;
                char *value_string = kson_string_parse(value);
                kmemory_free(value);
                karray->add(karray,value_string,KTYPE_HEAP);
            }else{
                value[i_cursor] = kson_array[cursor];
                i_cursor++;
            }
        }
    }
    return karray;
}
char *kson_array_pack(Karray *karray){
    int karray_length = 0;
    for(int a = 0 ; a < karray->length(karray) ; a++){
        karray_length += (strlen(karray->get(karray,a)));
    }
    char *pack = kmemory_alloc(sizeof(char)*karray_length*2+2+karray->length(karray)*3);
    memset(pack , 0 , sizeof(char)*karray_length*2);
    strcat(pack,"[");
    int length = karray->length(karray);
    for(int cursor = 0 ; cursor < length ; cursor++){
        char *value = kson_string_pack(karray->get(karray,cursor));
        strcat(pack,"\"");
        strcat(pack,value);
        strcat(pack,"\"");
        if(cursor < length - 1){
            strcat(pack,",");
        }
        kmemory_free(value);
    }
    strcat(pack,"]");
    return pack;
}