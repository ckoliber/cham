#include "cham_stream_node.h"

char *null_link = "0";
void *null_data = NULL;
Kmap *stream_map = NULL;

typedef struct StreamMessage{
    int file_fd;
    char *file_path;
    long file_seek;
} StreamMessage;

int equality(void *data_1 , void *data_2){
    return data_1 == data_2;
}

void key_destructor(void *data){}

void value_destructor(void *data){
    StreamMessage *stream_message = (StreamMessage*) data;
    close(stream_message->file_fd);
    free(stream_message->file_path);
    free(stream_message);
}

int onHandshake(KtcpPeer *peer , KtcpData *data){
    Kmap *handshake_map = kson_parse(data->data);
    if(handshake_map == NULL || kmap_indexof(handshake_map , "STREAM") == -1 || kmap_indexof(handshake_map , "ID") == -1 || kmap_indexof(handshake_map , "SCODE") == -1 || kmap_indexof(handshake_map , "LINK") == -1 || kmap_indexof(handshake_map , "SEEK") == -1 || !is_client(kmap_get(handshake_map , "ID") , kmap_get(handshake_map , "SCODE"))){
        return 0;
    }
    if(strcmp(kmap_get(handshake_map , "STREAM") , "GET") != 0 && strcmp(kmap_get(handshake_map , "STREAM") , "SET") != 0){
        return 0;
    }
    if(strcmp(kmap_get(handshake_map , "LINK") , null_link) == 0){
        Kmap *response_map = kson_parse("{}");
        void *seek = malloc(128);
        memset(seek , 0 , 128);
        sprintf(seek , "%d" , 0);
        response_map = kmap_put(response_map , "SEEK" , seek);
        char *response_message = kson_pack(response_map);
        if(write(peer->socket_fd , response_message , strlen(response_message) + 1) <= 0){
            return 0;
        }
        StreamMessage *stream_message = malloc(sizeof(StreamMessage));
        stream_message->file_path = NULL;
        stream_message->file_seek = 0;
        stream_message->file_fd = -1;
        stream_map = kmap_put(stream_map , (void *) peer->socket_fd, stream_message);
        return strcmp(kmap_get(handshake_map , "STREAM") , "SET") == 0 ? 2 : 1;
    }else{
        Kmap *link_map = link_parse(kmap_get(handshake_map , "LINK"));
        if(link_map == NULL){
            return 0;
        }
        char *file_path = link_make(link_map , strcmp(kmap_get(handshake_map , "STREAM") , "SET") == 0);
        if(file_path == NULL){
            return 0;
        }
        int file_fd = open(file_path , O_RDWR);
        long file_seek;
        if(strcmp(kmap_get(handshake_map , "STREAM") , "SET") == 0){
            file_seek = lseek(file_fd , 0 , SEEK_END);
        }else{
            file_seek = lseek(file_fd , atol(kmap_get(handshake_map , "SEEK")) , SEEK_SET);
        }
        if(file_seek < 0){
            return 0;
        }
        Kmap *response_map = kson_parse("{}");
        void *seek = malloc(128);
        memset(seek , 0 , 128);
        sprintf(seek , "%li" , file_seek);
        response_map = kmap_put(response_map , "SEEK" , seek);
        char *response_message = kson_pack(response_map);
        if(write(peer->socket_fd , response_message , strlen(response_message) + 1) <= 0){
            return 0;
        }
        StreamMessage *stream_message = malloc(sizeof(StreamMessage));
        stream_message->file_path = file_path;
        stream_message->file_seek = file_seek;
        stream_message->file_fd = open(file_path , O_RDWR);
        stream_map = kmap_put(stream_map , (void *) peer->socket_fd, stream_message);
        return strcmp(kmap_get(handshake_map , "STREAM") , "SET") == 0 ? 2 : 1;
    }
}

void onSpawn(){
    driver_connect();
}

void onOpen(KtcpPeer *peer){
    printf("ON OPEN FD %d\n",peer->socket_fd);fflush(stdout);
    ktcp_tune(peer , KTCP_TUNE_RDONLY);
}

void onReadable(KtcpPeer *peer , KtcpData *data){
    printf("ON READ FD %d = %s\n",peer->socket_fd,data->data);fflush(stdout);
    StreamMessage *stream_message = kmap_get(stream_map , (void *) peer->socket_fd);
    if(stream_message != NULL){
        if(stream_message->file_fd > 0){
            // data -> write to file
            data->data_size = (int) write(stream_message->file_fd , data->data , (size_t) data->data_size);
            if(data->data_size > 0){
                stream_message->file_seek += data->data_size;
                stream_map = kmap_refresh(stream_map , (void *) peer->socket_fd, stream_message);
            }else{
                ktcp_shutdown(peer);
            }
        }else{
            // null message -> bandwidth statistics
        }
    }else{
        // handshake message -> request
        int handshake = onHandshake(peer , data);
        printf("ON HANDSHAKE FD %d = %d\n",peer->socket_fd,handshake);fflush(stdout);
        if(handshake == 2){
            // set
            ktcp_tune(peer , KTCP_TUNE_RDONLY);
        }else if(handshake == 1){
            // get
            ktcp_tune(peer , KTCP_TUNE_WRONLY);
        }else{
            ktcp_shutdown(peer);
        }
    }
}

void onWritable(KtcpPeer *peer){
    printf("ON WRITE FD %d\n",peer->socket_fd);fflush(stdout);
    StreamMessage *stream_message = kmap_get(stream_map , (void *) peer->socket_fd);
    if(stream_message->file_fd > 0){
        // data -> write to file
        void *buffer = malloc((size_t) peer->ktcp->buffer_size);
        int buffer_length = (int) read(stream_message->file_fd , buffer , (size_t) peer->ktcp->buffer_size);
        if(buffer_length > 0){
            buffer_length = (int) write(peer->socket_fd , buffer , (size_t) buffer_length);
            stream_message->file_seek += buffer_length;
            stream_map = kmap_refresh(stream_map , (void *) peer->socket_fd, stream_message);
        }else{
            if(0){
                close(stream_message->file_fd);
                stream_message->file_fd = open(stream_message->file_path , O_RDWR);
                stream_map = kmap_refresh(stream_map , (void *) peer->socket_fd, stream_message);
            }else{
                ktcp_shutdown(peer);
            }
        }
    }else{
        // null message -> bandwidth statistics
        write(peer->socket_fd , null_data , MAXBUF);
    }
}

void onClose(KtcpPeer *peer){
    printf("ON CLOSE FD %d\n",peer->socket_fd);fflush(stdout);
    stream_map = kmap_remove(stream_map , (void *) peer->socket_fd);
}

void stream_node_stop(){
    printf("Stream node stops listening on port 1420\n");
    driver_disconnect();
    kill(getpid(),SIGKILL);
}

void stream_node_start(){
    int pool_size = 1;
    null_data = malloc(MAXBUF);
    signal(SIGKILL , (__sighandler_t) stream_node_stop);
    signal(SIGTERM , (__sighandler_t) stream_node_stop);
    stream_map = kmap_new(equality , key_destructor , value_destructor);
    Ktcp *ktcp = ktcp_new(MAXBUF);
    ktcp = ktcp_listen(
            ktcp,
            1,
            MAXCONNECTIONS,
            1420,
            onSpawn,
            onOpen,
            onReadable,
            onWritable,
            onClose
    );
    printf("Server is listening on port %i with pool size : %d\n",PORT,pool_size);
    wait(0);
    ktcp = ktcp_close(ktcp);
    ktcp_free(ktcp);
}

void loop_stop(){
    node_toggle(0);
    exit(0);
}

void loop_start(){
    int pid = fork();
    if(pid == 0){
        stream_node_start();
    }else{
        wait(0);
        loop_start();
    }
}

int main(int args,char **argv){
    signal(SIGKILL , (__sighandler_t) loop_stop);
    signal(SIGTERM , (__sighandler_t) loop_stop);
    node_toggle(1);
    loop_start();
}