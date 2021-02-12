#include "utils.h"

typedef struct Kudp_{
    struct Kudp interface;
    int pool_size;
    int buffer_size;
    void (*onSpawn)(struct Kudp *kudp);
    void (*onOpen)(struct Kudp *kudp , KudpPeer *kpeer , void *arg);
    void (*onData)(struct Kudp *kudp , KudpPeer *kpeer , char *data , int size);
    void (*onClose)(struct Kudp *kudp , KudpPeer *kpeer);
    int listen_port;
    Kprocessor *kprocessor;
    struct pollfd **poll;
} Kudp_;
typedef struct KudpArgs{
    Kudp_ *kudp_;
    int index;
} KudpArgs;

int kudp_listen(Kudp *kudp , char *name , int listen_port);
int kudp_binder(Kudp *kudp , char *name);
int kudp_bind(Kudp *kudp , void *arg , char *host , int port);
int kudp_sendto(Kudp *kudp , KudpPeer *kpeer , char *data , int size , int end);
int kudp_close(Kudp *kudp , KudpPeer *kpeer);
int kudp_shutdown(Kudp *kudp);

void* kudp_listen_loop(KudpArgs *args){
    args->kudp_->onSpawn((struct Kudp *) args->kudp_);
    int fd;
    struct sockaddr_in address;
    socklen_t address_length;
    address.sin_port = htons((uint16_t) args->kudp_->listen_port);
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = htonl(INADDR_ANY);
    if((fd = socket(AF_INET , SOCK_DGRAM , 0)) < 0){
        return NULL;
    }
    int option = 1;
    if(setsockopt(fd , SOL_SOCKET , SO_REUSEADDR , (char*)&option, sizeof(option)) < 0){
        return NULL;
    }
    fcntl (fd , F_SETFL, (fcntl (fd, F_GETFL, 0) | O_NONBLOCK));
    if(bind(fd , (struct sockaddr *) &address, sizeof(address)) < 0){
        return NULL;
    }
    args->kudp_->poll[0][0].fd = fd;
    args->kudp_->poll[0][0].events = POLLIN;
    int events;
    KudpPeer kpeer;
    char *buffer = kmemory_alloc((size_t) args->kudp_->buffer_size);
    int buffer_length;
    while(1){
        events = poll(args->kudp_->poll[args->index] , MAX_POLL_SET-1 , 1000);
        for (int cursor = 0 ; cursor < MAX_POLL_SET ; cursor++) {
            if(events <= 0){ break; }
            if ((fd = args->kudp_->poll[args->index][cursor].fd) < 0) {
                continue;
            }
            if (args->kudp_->poll[args->index][cursor].revents & POLLERR || args->kudp_->poll[args->index][cursor].revents & POLLHUP || args->kudp_->poll[args->index][cursor].revents & POLLNVAL) {
                kpeer.fd = fd;
                kpeer.index = cursor + MAX_POLL_SET*args->index;
                close(fd);
                args->kudp_->poll[args->index][cursor].fd = -1;
                args->kudp_->onClose((struct Kudp *) args->kudp_, &kpeer);
                return NULL;
            }else if(args->kudp_->poll[args->index][cursor].revents & POLLIN || args->kudp_->poll[args->index][cursor].revents & POLLPRI){
                while(1){
                    memset(&address , 0 , sizeof(struct sockaddr_in));
                    memset(&address , 0 , sizeof(socklen_t));
                    memset(buffer , 0 , (size_t) args->kudp_->buffer_size);
                    address_length = sizeof(address);
                    buffer_length = (int) recvfrom(fd, buffer , (size_t) args->kudp_->buffer_size, 0 , (struct sockaddr *) &address, &address_length);
                    if(buffer_length == -1){
                        break;
                    }else if(buffer_length == 0){
                        // close -> socket_fd
                        kpeer.fd = fd;
                        kpeer.index = cursor + MAX_POLL_SET*args->index;
                        close(fd);
                        args->kudp_->poll[args->index][cursor].fd = -1;
                        args->kudp_->onClose((struct Kudp *) args->kudp_, &kpeer);
                        return NULL;
                    }else{
                        // recv -> socket_fd , host , port
                        kpeer.host = inet_ntoa(address.sin_addr);
                        kpeer.port = ntohs(address.sin_port);
                        kpeer.fd = fd;
                        kpeer.index = cursor + MAX_POLL_SET*args->index;
                        args->kudp_->onData((struct Kudp *) args->kudp_, &kpeer, buffer, buffer_length);
                    }
                }
            }
            if (--events <= 0) {
                break;
            }
        }
    }
    kmemory_free(buffer);
    kmemory_free(args);
    return NULL;
}
void* kudp_binder_loop(KudpArgs *args){
    args->kudp_->onSpawn((struct Kudp *) args->kudp_);
    for(int cursor = 0 ; cursor < MAX_POLL_SET ; cursor++){
        args->kudp_->poll[args->index][cursor].fd = -1;
    }
    struct sockaddr_in client_address;
    socklen_t client_length;
    int fd;
    int events;
    KudpPeer kpeer;
    uint8_t *buffer = kmemory_alloc((size_t) args->kudp_->buffer_size);
    int buffer_length;
    while(1){
        events = poll(args->kudp_->poll[args->index] , MAX_POLL_SET-1 , 1000);
        for (int cursor = 0 ; cursor < MAX_POLL_SET ; cursor++) {
            if(events <= 0){ break; }
            if ((fd = args->kudp_->poll[args->index][cursor].fd) < 0) {
                continue;
            }
            if (args->kudp_->poll[args->index][cursor].revents & POLLERR || args->kudp_->poll[args->index][cursor].revents & POLLHUP || args->kudp_->poll[args->index][cursor].revents & POLLNVAL) {
                kpeer.fd = fd;
                kpeer.index = cursor + MAX_POLL_SET*args->index;
                close(fd);
                args->kudp_->poll[args->index][cursor].fd = -1;
                args->kudp_->onClose((struct Kudp *) args->kudp_, &kpeer);
            }else if(args->kudp_->poll[args->index][cursor].revents & POLLIN || args->kudp_->poll[args->index][cursor].revents & POLLPRI){
                while(1){
                    memset(&client_address , 0 , sizeof(struct sockaddr_in));
                    memset(&client_length , 0 , sizeof(socklen_t));
                    memset(buffer , 0 , (size_t) args->kudp_->buffer_size);
                    client_length = sizeof(client_address);
                    buffer_length = (int) recvfrom(fd, buffer , (size_t) args->kudp_->buffer_size, 0 , (struct sockaddr *) &client_address, &client_length);
                    if(buffer_length == -1){
                        break;
                    }else if(buffer_length == 0){
                        // close -> socket_fd
                        kpeer.fd = fd;
                        kpeer.index = cursor + MAX_POLL_SET*args->index;
                        close(fd);
                        args->kudp_->poll[args->index][cursor].fd = -1;
                        args->kudp_->onClose((struct Kudp *) args->kudp_, &kpeer);
                    }else{
                        // recv -> socket_fd , host , port
                        kpeer.host = inet_ntoa(client_address.sin_addr);
                        kpeer.port = ntohs(client_address.sin_port);
                        kpeer.fd = fd;
                        kpeer.index = cursor + MAX_POLL_SET*args->index;
                        args->kudp_->onData((struct Kudp *) args->kudp_, &kpeer, (char *) buffer, buffer_length);
                    }
                }
            }
            if (--events <= 0) {
                break;
            }
        }
    }
    kmemory_free(buffer);
    kmemory_free(args);
    return NULL;
}

int kudp_listen(struct Kudp *kudp , char *name , int listen_port){
    Kudp_ *kudp_ = (Kudp_ *) kudp;
    if(kudp_ == NULL){
        return 0;
    }
    if(kudp_->kprocessor != NULL || kudp_->poll != NULL){
        return 0;
    }
    kudp_->listen_port = listen_port;
    kudp_->kprocessor = kprocessor_new(KPROCESSOR_TYPE_THREAD , 1 , name , NULL);
    if(kudp_->kprocessor == NULL){
        return 0;
    }
    kudp_->poll = kmemory_alloc(sizeof(struct pollfd*));
    kudp_->poll[0] = kmemory_alloc(sizeof(struct pollfd));
    kudp_->kprocessor->start(kudp_->kprocessor);
    KudpArgs *args = kmemory_alloc(sizeof(KudpArgs));
    args->kudp_ = kudp_;
    args->index = 0;
    kudp_->kprocessor->post(kudp_->kprocessor , (void *(*)(void *)) kudp_listen_loop, args);
    return 1;
}
int kudp_binder(struct Kudp *kudp , char *name){
    Kudp_ *kudp_ = (Kudp_ *) kudp;
    if(kudp_ == NULL){
        return 0;
    }
    if(kudp_->kprocessor != NULL || kudp_->poll != NULL){
        return 0;
    }
    kudp_->listen_port = -1;
    kudp_->kprocessor = kprocessor_new(KPROCESSOR_TYPE_THREAD , kudp_->pool_size , name , NULL);
    if(kudp_->kprocessor == NULL){
        return 0;
    }
    kudp_->poll = kmemory_alloc(kudp_->pool_size*sizeof(struct pollfd*));
    for(int cursor = 0 ; cursor < kudp_->pool_size ; cursor++){
        kudp_->poll[cursor] = kmemory_alloc(MAX_POLL_SET*sizeof(struct pollfd));
    }
    kudp_->kprocessor->start(kudp_->kprocessor);
    for(int cursor = 0 ; cursor < kudp_->pool_size ; cursor++){
        KudpArgs *args = kmemory_alloc(sizeof(KudpArgs));
        args->kudp_ = kudp_;
        args->index = cursor;
        kudp_->kprocessor->post(kudp_->kprocessor , (void *(*)(void *)) kudp_binder_loop, args);
    }
    return 1;
}
int kudp_bind(struct Kudp *kudp , void *arg , char *host , int port){
    Kudp_ *kudp_ = (Kudp_ *) kudp;
    if(kudp_ == NULL || kudp_->poll == NULL){
        return 0;
    }
    if(kudp_->kprocessor == NULL || kudp_->poll == NULL){
        return 0;
    }
    int fd;
    if((fd = socket(AF_INET , SOCK_DGRAM , 0)) < 0){
        return 0;
    }
    int option = 1;
    if(setsockopt(fd , SOL_SOCKET , SO_REUSEADDR , (char*)&option, sizeof(option)) < 0){
        return 0;
    }
    fcntl (fd , F_SETFL, (fcntl (fd, F_GETFL, 0) | O_NONBLOCK));
    int processor_index = 0;
    int processor_index_size = INT_MAX;
    for(int cursor = 0 ; cursor < kudp_->pool_size ; cursor++){
        int index_size = 0;
        for(int index = 0 ; index < MAX_POLL_SET ; index++){
            if(kudp_->poll[cursor][index].fd != -1){
                index_size++;
            }
        }
        if(index_size < processor_index_size){
            processor_index = cursor;
            processor_index_size = index_size;
        }
    }
    for(processor_index_size = 0 ; processor_index_size < MAX_POLL_SET ; processor_index_size++){
        if(kudp_->poll[processor_index][processor_index_size].fd == -1){
            kudp_->poll[processor_index][processor_index_size].fd = fd;
            kudp_->poll[processor_index][processor_index_size].events = POLLIN;
            break;
        }
    }
    KudpPeer kpeer;
    kpeer.index = processor_index*MAX_POLL_SET + processor_index_size;
    kpeer.fd = fd;
    kpeer.host = host;
    kpeer.port = port;
    kudp_->onOpen(kudp , &kpeer , arg);
    return 1;
}
int kudp_sendto(struct Kudp *kudp , KudpPeer *kpeer , char *data , int size , int end){
    Kudp_ *kudp_ = (Kudp_ *) kudp;
    if(kudp_ == NULL){
        return -1;
    }
    if(kpeer == NULL){
        return -1;
    }
    int send = 0;
    struct hostent *server = gethostbyname(kpeer->host);
    struct sockaddr_in address;
    if(server != NULL){
        memset(&address,0,sizeof(address));
        address.sin_port = htons((uint16_t) kpeer->port);
        address.sin_family = AF_INET;
        memcpy(&address.sin_addr.s_addr,server->h_addr,(size_t)server->h_length);
        size_t send_length = (size_t) (size < kudp_->buffer_size ? size : kudp_->buffer_size);
        int pointer_length  = size;
        uint8_t *pointer_address = (uint8_t *) data;
        while (pointer_length > 0) {
            if(end > 0){
                pointer_address[send_length - 1] = (uint8_t) end;
            }
            ssize_t len = sendto(kpeer->fd , pointer_address, send_length , 0, (struct sockaddr *) &address, sizeof(address));
            if (len < 0){
                break;
            }
            send += len;
            pointer_address += len;
            pointer_length -= len;
            send_length = (size_t) (pointer_length < kudp_->buffer_size ? pointer_length : kudp_->buffer_size);
        }
    }
    return send;
}
int kudp_close(struct Kudp *kudp , KudpPeer *kpeer){
    Kudp_ *kudp_ = (Kudp_ *) kudp;
    if(kudp_ == NULL){
        return 0;
    }
    if(kpeer == NULL){
        return 0;
    }
    if(kudp_->kprocessor == NULL || kudp_->poll == NULL){
        return 0;
    }
    int processor_index = kpeer->index / MAX_POLL_SET;
    int poll_index = kpeer->index - processor_index*MAX_POLL_SET;
    kudp_->poll[processor_index][poll_index].fd = -1;
    close(kpeer->fd);
    kudp_->onClose(kudp , kpeer);
    return 1;
}
int kudp_shutdown(struct Kudp *kudp){
    Kudp_ *kudp_ = (Kudp_ *) kudp;
    if(kudp_ == NULL){
        return 0;
    }
    if(kudp_->kprocessor != NULL){
        kudp_->kprocessor->stop(kudp_->kprocessor);
        kprocessor_free(kudp_->kprocessor);
        kudp_->kprocessor = NULL;
    }
    if(kudp_->poll != NULL){
        for(int cursor = 0 ; cursor < kudp_->pool_size ; cursor++){
            if(kudp_->poll[cursor] != NULL){
                close(kudp_->poll[cursor]->fd);
                kmemory_free(kudp_->poll[cursor]);
            }
        }
        kmemory_free(kudp_->poll);
        kudp_->poll = NULL;
    }
    return 1;
}

Kudp *kudp_new(
        int pool_size,
        int buffer_size,
        void (*onSpawn)(struct Kudp *kudp),
        void (*onOpen)(struct Kudp *kudp , KudpPeer *kpeer , void *arg),
        void (*onData)(struct Kudp *kudp , KudpPeer *kpeer , char *data , int size),
        void (*onClose)(struct Kudp *kudp , KudpPeer *kpeer)
){
    Kudp_ *kudp_ = kmemory_alloc(sizeof(Kudp_));
    kudp_->interface.listen = kudp_listen;
    kudp_->interface.binder = kudp_binder;
    kudp_->interface.bind = kudp_bind;
    kudp_->interface.sendto = kudp_sendto;
    kudp_->interface.close = kudp_close;
    kudp_->interface.shutdown = kudp_shutdown;
    kudp_->pool_size = pool_size;
    kudp_->buffer_size = buffer_size;
    kudp_->onSpawn = onSpawn;
    kudp_->onOpen = onOpen;
    kudp_->onData = onData;
    kudp_->onClose = onClose;
    kudp_->listen_port = 0;
    kudp_->kprocessor = NULL;
    kudp_->poll = NULL;
    return (Kudp *) kudp_;
}
void kudp_free(Kudp *kudp){
    Kudp_ *kudp_ = (Kudp_ *) kudp;
    if(kudp_ == NULL){
        return;
    }
    kudp_shutdown(kudp);
    kmemory_free(kudp_);
}