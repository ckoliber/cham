// Peer to Peer

#include "utils.h"

typedef enum CasterType{
    CASTER_TUNE,
    CASTER_CAST
} CasterType;
typedef enum CasterTune{
    CASTER_TUNE_OPEN,    // connecting ( wait on connecting )
    CASTER_TUNE_RDWR,       // casting -> read , write
    CASTER_TUNE_RDONLY,     // casting -> read
    CASTER_TUNE_WRONLY,     // casting -> write
    CASTER_TUNE_NONE,       // casting -> no data
    CASTER_TUNE_WAIT,       // casting -> waiting change tune
    CASTER_TUNE_CLOSE, // disconnected ( closed || tuned || response )
} CasterTune;
typedef struct Caster{
    int (*start)(struct Caster *caster , char *setting_id , char *setting_scode);
    Kmap* (*state)(struct Caster *caster);
    int (*stop)(struct Caster *caster);
    int (*tune)(struct Caster *caster , char *target_id , CasterTune tune);
    int (*cast)(struct Caster *caster , void *data , int size , int end);
    void *bundle;
} Caster;

Caster *caster_new(
        int pool_size,
        int buffer_size,
        char *caster_host,
        int caster_port,
        void (*onTune)(struct Caster *caster , char *target_id , CasterTune tune),
        void (*onCast)(struct Caster *caster , char *target_id , void *data , int size)
);
void caster_free(Caster *caster);