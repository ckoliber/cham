#include "utils.h"

typedef struct Ktcp_{
    struct Ktcp interface;
    int pool_size;
    int buffer_size;
    void (*onSpawn)(struct Ktcp *ktcp);
    void (*onOpen)(struct Ktcp *ktcp , KtcpPeer *kpeer , void *arg);
    void (*onReadable)(struct Ktcp *ktcp , KtcpPeer *kpeer , char *data , int size);
    void (*onWritable)(struct Ktcp *ktcp , KtcpPeer *kpeer);
    void (*onClose)(struct Ktcp *ktcp , KtcpPeer *kpeer);
    int max_connections;
    int listen_port;
    Kprocessor *kprocessor;
    struct pollfd **poll;
    int epoll;
} Ktcp_;
typedef struct KtcpArgs{
    int index;
    Ktcp_ *ktcp_;
} KtcpArgs;

int ktcp_listen(struct Ktcp *ktcp , char *name , int max_connections , int listen_port);
int ktcp_connectivity(struct Ktcp *ktcp , char *name);
int ktcp_connect(struct Ktcp *ktcp , void *arg , char *host , int port);
int ktcp_tune(struct Ktcp *ktcp , KtcpPeer *kpeer , KtcpTune ktune);
char* ktcp_gethost(struct Ktcp *ktcp , KtcpPeer *kpeer);
int ktcp_getport(struct Ktcp *ktcp , KtcpPeer *kpeer);
int ktcp_send(struct Ktcp *ktcp , KtcpPeer *kpeer , char *data , int size);
int ktcp_close(struct Ktcp *ktcp , KtcpPeer *kpeer);
int ktcp_shutdown(struct Ktcp *ktcp);

void* ktcp_listen_loop(KtcpArgs *args){
#if defined(__unix__) && !defined(__ANDROID__)
    args->ktcp_->onSpawn((struct Ktcp *) args->ktcp_);
    int listen_fd;
    int option;
    struct sockaddr_in address;
    socklen_t address_length;
    address.sin_port = htons((uint16_t) args->ktcp_->listen_port);
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = htonl(INADDR_ANY);
    if((listen_fd = socket(AF_INET , SOCK_STREAM , IPPROTO_TCP)) < 0){
        return NULL;
    }
    option = 1;
    if(setsockopt(listen_fd , SOL_SOCKET , SO_REUSEADDR , (char*)&option, sizeof(option)) < 0){
        return NULL;
    }
    option = 1;
    if (setsockopt(listen_fd, SOL_SOCKET, SO_REUSEPORT, (char*)&option, sizeof(option)) < 0){
        return NULL;
    }
    option = fcntl (listen_fd, F_GETFL, 0);
    if (option == -1) {
        return NULL;
    }
    option |= O_NONBLOCK;
    if (fcntl (listen_fd , F_SETFL, option) == -1) {
        return NULL;
    }
    if(bind(listen_fd , (struct sockaddr *) &address, sizeof(address)) < 0){
        return NULL;
    }
    if(listen(listen_fd , args->ktcp_->max_connections) < 0){
        return NULL;
    }
    memset(&address , 0 , sizeof(struct sockaddr_in));
    address_length = sizeof(struct sockaddr_in);
    int events;
    struct epoll_event event;
    struct epoll_event epoll_events[MAX_EPOLL_EVENTS];
    if((args->ktcp_->epoll = epoll_create(args->ktcp_->max_connections)) < 0){
        return NULL;
    }
    event.data.fd = listen_fd;
    event.events = EPOLLIN | EPOLLET;
    epoll_ctl(args->ktcp_->epoll , EPOLL_CTL_ADD , listen_fd , &event);
    KtcpPeer kpeer;
    kpeer.index = -1;
    char *buffer = kmemory_alloc((size_t) args->ktcp_->buffer_size);
    int buffer_length;
    while(1){
        events = epoll_wait(args->ktcp_->epoll,epoll_events,MAX_EPOLL_EVENTS,-1);
        while(events > 0){
            events--;
            if((epoll_events[events].events & EPOLLERR) || (epoll_events[events].events & EPOLLHUP)){
                kpeer.fd = epoll_events[events].data.fd;
                epoll_ctl (args->ktcp_->epoll,EPOLL_CTL_DEL,epoll_events[events].data.fd, NULL);
                close(epoll_events[events].data.fd);
                args->ktcp_->onClose((struct Ktcp *) args->ktcp_, &kpeer);
            }else if(epoll_events[events].data.fd == listen_fd){
                while(1){
                    memset(&address , 0 , sizeof(address));
                    int accept_fd = accept(listen_fd,(struct sockaddr *) &address,&address_length);
                    if(accept_fd < 0){
                        break;
                    }
                    fcntl (accept_fd,F_SETFL,(fcntl (accept_fd, F_GETFL, 0) | O_NONBLOCK));
                    event.data.fd = accept_fd;
                    event.events = EPOLLIN | EPOLLET;
                    epoll_ctl (args->ktcp_->epoll,EPOLL_CTL_ADD,accept_fd, &event);
                    kpeer.fd = accept_fd;
                    args->ktcp_->onOpen((struct Ktcp *) args->ktcp_, &kpeer, NULL);
                }
            }else if(epoll_events[events].events & EPOLLIN){
                while(1){
                    memset(buffer , 0 , (size_t) args->ktcp_->buffer_size);
                    buffer_length = (int) read(epoll_events[events].data.fd,buffer,(size_t) args->ktcp_->buffer_size);
                    if(buffer_length == -1){
                        break;
                    }else if(buffer_length == 0){
                        kpeer.fd = epoll_events[events].data.fd;
                        epoll_ctl (args->ktcp_->epoll,EPOLL_CTL_DEL,epoll_events[events].data.fd,NULL);
                        close(epoll_events[events].data.fd);
                        args->ktcp_->onClose((struct Ktcp *) args->ktcp_, &kpeer);
                    }else{
                        kpeer.fd = epoll_events[events].data.fd;
                        args->ktcp_->onReadable((struct Ktcp *) args->ktcp_, &kpeer, buffer, buffer_length);
                    }
                }
            }else if(epoll_events[events].events & EPOLLOUT){
                kpeer.fd = epoll_events[events].data.fd;
                args->ktcp_->onWritable((struct Ktcp *) args->ktcp_, &kpeer);
            }
        }
    }
    kmemory_free(buffer);
    kmemory_free(args);
#endif
    return NULL;
}
void* ktcp_connectivity_loop(KtcpArgs *args){
    args->ktcp_->onSpawn((struct Ktcp *) args->ktcp_);
    for(int cursor = 0 ; cursor < MAX_POLL_SET ; cursor++){
        args->ktcp_->poll[args->index][cursor].fd = -1;
    }
    int socket_fd;
    int events;
    KtcpPeer kpeer;
    char *buffer = kmemory_alloc((size_t) args->ktcp_->buffer_size);
    int buffer_length;
    while(1){
        events = poll(args->ktcp_->poll[args->index] , MAX_POLL_SET-1 , 100);
        for (int cursor = 0 ; cursor < MAX_POLL_SET ; cursor++) {
            if ((socket_fd = args->ktcp_->poll[args->index][cursor].fd) < 0) {
                continue;
            }
            if (args->ktcp_->poll[args->index][cursor].revents & POLLERR) {
                kpeer.fd = socket_fd;
                kpeer.index = cursor + MAX_POLL_SET*args->index;
                close(socket_fd);
                args->ktcp_->poll[args->index][cursor].fd = -1;
                args->ktcp_->onClose((struct Ktcp *) args->ktcp_, &kpeer);
            }else if(args->ktcp_->poll[args->index][cursor].revents & POLLIN){
                while(1){
                    memset(buffer , 0 , (size_t) args->ktcp_->buffer_size);
                    buffer_length = (int) read(socket_fd, buffer, (size_t) args->ktcp_->buffer_size);
                    if(buffer_length == -1){
                        break;
                    }else if(buffer_length == 0){
                        kpeer.fd = socket_fd;
                        kpeer.index = cursor + MAX_POLL_SET*args->index;
                        close(socket_fd);
                        args->ktcp_->poll[args->index][cursor].fd = -1;
                        args->ktcp_->onClose((struct Ktcp *) args->ktcp_, &kpeer);
                    }else{
                        kpeer.fd = socket_fd;
                        kpeer.index = cursor + MAX_POLL_SET*args->index;
                        args->ktcp_->onReadable((struct Ktcp *) args->ktcp_, &kpeer, buffer, buffer_length);
                    }
                }
            }else if(args->ktcp_->poll[args->index][cursor].revents & POLLOUT){
                kpeer.fd = socket_fd;
                kpeer.index = cursor + MAX_POLL_SET*args->index;
                args->ktcp_->onWritable((struct Ktcp *) args->ktcp_, &kpeer);
            }
            if (--events <= 0) {
                break;
            }
        }
        if(args->ktcp_->interface.bundle != NULL){
            args->ktcp_->onWritable((struct Ktcp *) args->ktcp_, NULL);
        }
    }
    kmemory_free(buffer);
    kmemory_free(args);
    return NULL;
}

int ktcp_listen(struct Ktcp *ktcp , char *name , int max_connections , int listen_port){
    Ktcp_ *ktcp_ = (Ktcp_ *) ktcp;
    if(ktcp_ == NULL){
        return 0;
    }
#if defined(__unix__) && !defined(__ANDROID__)
    if(ktcp_->kprocessor != NULL || ktcp_->poll != NULL || ktcp_->epoll > 0){
        return 0;
    }
    ktcp_->max_connections = max_connections;
    ktcp_->listen_port = listen_port;
    ktcp_->kprocessor = kprocessor_new(KPROCESSOR_TYPE_PROCESS , ktcp_->pool_size , name , NULL);
    if(ktcp_->kprocessor == NULL){
        return 0;
    }
    ktcp_->kprocessor->start(ktcp_->kprocessor);
    for(int cursor = 0 ; cursor < ktcp_->pool_size ; cursor++){
        KtcpArgs *args = kmemory_alloc(sizeof(KtcpArgs));
        args->ktcp_ = ktcp_;
        args->index = cursor;
        ktcp_->kprocessor->post(ktcp_->kprocessor , (void *(*)(void *)) ktcp_listen_loop, args);
    }
    return 1;
#else
    return 0;
#endif
}
int ktcp_connectivity(struct Ktcp *ktcp , char *name){
    Ktcp_ *ktcp_ = (Ktcp_ *) ktcp;
    if(ktcp_ == NULL){
        return 0;
    }
    if(ktcp_->kprocessor != NULL || ktcp_->poll != NULL || ktcp_->epoll > 0){
        return 0;
    }
    ktcp_->kprocessor = kprocessor_new(KPROCESSOR_TYPE_THREAD , ktcp_->pool_size , name , NULL);
    if(ktcp_->kprocessor == NULL){
        return 0;
    }
    ktcp_->poll = kmemory_alloc(ktcp_->pool_size*sizeof(struct pollfd*));
    for(int cursor = 0 ; cursor < ktcp_->pool_size ; cursor++){
        ktcp_->poll[cursor] = kmemory_alloc(MAX_POLL_SET*sizeof(struct pollfd));
    }
    ktcp_->kprocessor->start(ktcp_->kprocessor);
    for(int cursor = 0 ; cursor < ktcp_->pool_size ; cursor++){
        KtcpArgs *args = kmemory_alloc(sizeof(KtcpArgs));
        args->ktcp_ = ktcp_;
        args->index = cursor;
        ktcp_->kprocessor->post(ktcp_->kprocessor , (void *(*)(void *)) ktcp_connectivity_loop, args);
    }
    return 1;
}
int ktcp_connect(struct Ktcp *ktcp , void *arg , char *host , int port){
    Ktcp_ *ktcp_ = (Ktcp_ *) ktcp;
    if(ktcp_ == NULL) {
        return 0;
    }
    if(ktcp_->kprocessor == NULL || ktcp_->poll == NULL){
        return 0;
    }
    int fd;
    struct sockaddr_in address;
    address.sin_port = htons((uint16_t) port);
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = inet_addr(host);
    if((fd = socket(AF_INET , SOCK_STREAM , 0)) < 0){
        return 0;
    }
    if(connect(fd , (struct sockaddr *) &address, sizeof(address)) < 0){
        return 0;
    }
    fcntl (fd , F_SETFL, (fcntl (fd, F_GETFL, 0) | O_NONBLOCK));
    int processor_index = 0;
    int processor_index_size = INT_MAX;
    for(int cursor = 0 ; cursor < ktcp_->pool_size ; cursor++){
        int index_size = 0;
        for(int index = 0 ; index < MAX_POLL_SET ; index++){
            if(ktcp_->poll[cursor][index].fd != -1){
                index_size++;
            }
        }
        if(index_size < processor_index_size){
            processor_index = cursor;
            processor_index_size = index_size;
        }
    }
    for(processor_index_size = 0 ; processor_index_size < MAX_POLL_SET ; processor_index_size++){
        if(ktcp_->poll[processor_index][processor_index_size].fd == -1){
            ktcp_->poll[processor_index][processor_index_size].fd = fd;
            ktcp_->poll[processor_index][processor_index_size].events = POLLIN;
            break;
        }
    }
    KtcpPeer kpeer;
    kpeer.index = processor_index*MAX_POLL_SET + processor_index_size;
    kpeer.fd = fd;
    ktcp_->onOpen(ktcp, &kpeer, arg);
    return 1;
}
int ktcp_tune(struct Ktcp *ktcp , KtcpPeer *kpeer , KtcpTune ktune){
    Ktcp_ *ktcp_ = (Ktcp_ *) ktcp;
    if(ktcp_ == NULL){
        return 0;
    }
    if(kpeer == NULL){
        return 0;
    }
    if(kpeer->index == -1){
#if defined(__unix__) && !defined(__ANDROID__)
        if(ktcp_->kprocessor == NULL || ktcp_->epoll <= 0){
            return 0;
        }
        struct epoll_event event;
        event.data.fd = kpeer->fd;
        switch (ktune){
            case KTCP_TUNE_RDWR:
                event.events = EPOLLIN | EPOLLOUT;
                epoll_ctl(ktcp_->epoll , EPOLL_CTL_DEL , kpeer->fd , NULL);
                epoll_ctl(ktcp_->epoll , EPOLL_CTL_ADD , kpeer->fd , &event);
                break;
            case KTCP_TUNE_RDONLY:
                event.events = EPOLLIN | EPOLLET;
                epoll_ctl(ktcp_->epoll , EPOLL_CTL_DEL , kpeer->fd , NULL);
                epoll_ctl(ktcp_->epoll , EPOLL_CTL_ADD , kpeer->fd , &event);
                break;
            case KTCP_TUNE_WRONLY:
                event.events = EPOLLOUT;
                epoll_ctl(ktcp_->epoll , EPOLL_CTL_DEL , kpeer->fd , NULL);
                epoll_ctl(ktcp_->epoll , EPOLL_CTL_ADD , kpeer->fd , &event);
                break;
            case KTCP_TUNE_NONE:
                epoll_ctl(ktcp_->epoll , EPOLL_CTL_DEL , kpeer->fd , NULL);
                break;
        }
        return 1;
#else
        return 0;
#endif
    }else{
        if(ktcp_->kprocessor == NULL || ktcp_->poll == NULL){
            return 0;
        }
        int processor_index = kpeer->index / MAX_POLL_SET;
        int poll_index = kpeer->index - processor_index*MAX_POLL_SET;
        switch (ktune){
            case KTCP_TUNE_RDWR:
                ktcp_->poll[processor_index][poll_index].events = POLLIN | POLLOUT;
                break;
            case KTCP_TUNE_RDONLY:
                ktcp_->poll[processor_index][poll_index].events = POLLIN;
                break;
            case KTCP_TUNE_WRONLY:
                ktcp_->poll[processor_index][poll_index].events = POLLOUT;
                break;
            case KTCP_TUNE_NONE:
                ktcp_->poll[processor_index][poll_index].events = 0;
                break;
        }
        return 1;
    }
}
char* ktcp_gethost(struct Ktcp *ktcp , KtcpPeer *kpeer){
    Ktcp_ *ktcp_ = (Ktcp_ *) ktcp;
    if(ktcp_ == NULL){
        return NULL;
    }
    if(kpeer == NULL){
        return NULL;
    }
    struct sockaddr_in address;
    socklen_t length = sizeof(address);
    if (getsockname(kpeer->fd , (struct sockaddr *)&address, &length) == -1){
        return NULL;
    }
    return inet_ntoa(address.sin_addr);
}
int ktcp_getport(struct Ktcp *ktcp , KtcpPeer *kpeer){
    Ktcp_ *ktcp_ = (Ktcp_ *) ktcp;
    if(ktcp_ == NULL){
        return 0;
    }
    if(kpeer == NULL){
        return 0;
    }
    struct sockaddr_in address;
    socklen_t length = sizeof(address);
    if (getsockname(kpeer->fd , (struct sockaddr *)&address, &length) == -1){
        return 0;
    }
    return ntohs(address.sin_port);
}
int ktcp_send(struct Ktcp *ktcp , KtcpPeer *kpeer , char *data , int size){
    Ktcp_ *ktcp_ = (Ktcp_ *) ktcp;
    if(ktcp_ == NULL){
        return -1;
    }
    if(kpeer == NULL){
        return -1;
    }
    return (int) write(kpeer->fd , data , (size_t) size);
}
int ktcp_close(struct Ktcp *ktcp , KtcpPeer *kpeer){
    Ktcp_ *ktcp_ = (Ktcp_ *) ktcp;
    if(ktcp_ == NULL){
        return 0;
    }
    if(kpeer == NULL){
        return 0;
    }
    if(kpeer->index == -1){
#if defined(__unix__) && !defined(__ANDROID__)
        if(ktcp_->kprocessor == NULL || ktcp_->epoll <= 0){
            return 0;
        }
        epoll_ctl(ktcp_->epoll , EPOLL_CTL_DEL , kpeer->fd , NULL);
#else
        return 0;
#endif
    }else{
        if(ktcp_->kprocessor == NULL || ktcp_->poll == NULL){
            return 0;
        }
        int processor_index = kpeer->index / MAX_POLL_SET;
        int poll_index = kpeer->index - processor_index*MAX_POLL_SET;
        ktcp_->poll[processor_index][poll_index].fd = -1;
    }
    close(kpeer->fd);
    ktcp_->onClose(ktcp , kpeer);
    return 1;
}
int ktcp_shutdown(struct Ktcp *ktcp){
    Ktcp_ *ktcp_ = (Ktcp_ *) ktcp;
    if(ktcp_ == NULL){
        return 0;
    }
    if(ktcp_->kprocessor != NULL){
        ktcp_->kprocessor->stop(ktcp_->kprocessor);
        kprocessor_free(ktcp_->kprocessor);
        ktcp_->kprocessor = NULL;
    }
    if(ktcp_->poll != NULL){
        for(int cursor = 0 ; cursor < ktcp_->pool_size ; cursor++){
            if(ktcp_->poll[cursor] != NULL){
                close(ktcp_->poll[cursor]->fd);
                kmemory_free(ktcp_->poll[cursor]);
            }
        }
        kmemory_free(ktcp_->poll);
        ktcp_->poll = NULL;
    }
    if(ktcp_->epoll > 0){
        close(ktcp_->epoll);
        ktcp_->epoll = -1;
    }
    return 1;
}

Ktcp *ktcp_new(
        int pool_size,
        int buffer_size ,
        void (*onSpawn)(Ktcp *ktcp) ,
        void (*onOpen)(Ktcp *ktcp , KtcpPeer *kpeer , void *arg) ,
        void (*onReadable)(Ktcp *ktcp , KtcpPeer *kpeer , char *data , int size) ,
        void (*onWritable)(Ktcp *ktcp , KtcpPeer *kpeer) ,
        void (*onClose)(Ktcp *ktcp , KtcpPeer *kpeer)
){
    Ktcp_ *ktcp_ = kmemory_alloc(sizeof(Ktcp_));
    ktcp_->interface.listen = ktcp_listen;
    ktcp_->interface.connectivity = ktcp_connectivity;
    ktcp_->interface.connect = ktcp_connect;
    ktcp_->interface.tune = ktcp_tune;
    ktcp_->interface.gethost = ktcp_gethost;
    ktcp_->interface.getport = ktcp_getport;
    ktcp_->interface.send = ktcp_send;
    ktcp_->interface.close = ktcp_close;
    ktcp_->interface.shutdown = ktcp_shutdown;
    ktcp_->pool_size = pool_size;
    ktcp_->buffer_size = buffer_size;
    ktcp_->onSpawn = onSpawn;
    ktcp_->onOpen = onOpen;
    ktcp_->onReadable = onReadable;
    ktcp_->onWritable = onWritable;
    ktcp_->onClose = onClose;
    ktcp_->max_connections = 0;
    ktcp_->listen_port = 0;
    ktcp_->kprocessor = NULL;
    ktcp_->poll = NULL;
    ktcp_->epoll = -1;
    return (Ktcp *) ktcp_;
}
void ktcp_free(Ktcp *ktcp){
    Ktcp_ *ktcp_ = (Ktcp_ *) ktcp;
    if(ktcp_ == NULL){
        return;
    }
    ktcp_shutdown(ktcp);
    kmemory_free(ktcp_);
}