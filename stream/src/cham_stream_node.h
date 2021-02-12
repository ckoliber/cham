#ifndef CHAMSERVER_CHAM_STREAM_NODE_H
#define CHAMSERVER_CHAM_STREAM_NODE_H

#define PORT		    1420
#define MAXBUF		    4096
#define MAXCONNECTIONS  1000000
#define MAX_BYTE_RATE   1048576 // 1 MBPS

#include "sys/inotify.h"
#include "poll.h"
#include "errno.h"
#include <sys/wait.h>
#include "cham_util.h"
#include "klb/net/ktcp.h"
#include "klb/dsa/kmap.h"
#include "klb/net/kpipe.h"
#include "fcntl.h"

void stream_node_stop();
void stream_node_start();

#endif //CHAMSERVER_CHAM_STREAM_NODE_H
