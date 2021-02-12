#ifndef KLB_KPIPE_H
#define KLB_KPIPE_H

#include "kprocessor.h"

/// Tip : Byte rate limitation works on reading stream !!!

#define KPIPE_BYTERATE_MAX -1

typedef enum KpipeAutoClose{
    KPIPE_CLOSE_FDS,
    KPIPE_OPEN_FDS
} KpipeAutoClose;

typedef enum KpipeProcessorType{
    KPIPE_PROCESSOR_NONE,     // run pipe in this process
    KPIPE_PROCESSOR_THREAD,   // run pipe in another thread
    KPIPE_PROCESSOR_PROCESS   // run pipe in another process
} KpipeProcessorType;

typedef struct Kpipe{
    int shared_id;
    int input_fd;
    int output_fd;
    double max_byterate;
    double buffer_size;
    KpipeAutoClose auto_close;
    Kprocessor *processor;
} Kpipe;

Kpipe *kpipe_new(int shared_key , KpipeProcessorType processor_type , double max_byterate , double buffer_size , int input_fd , int output_fd);

void kpipe_start(Kpipe *kpipe , KpipeAutoClose auto_close);

void kpipe_stop(Kpipe *kpipe);

void kpipe_free(Kpipe *kpipe);

#endif //KLB_KPIPE_H
