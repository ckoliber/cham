#include <jni.h>
#include <interface.h>

typedef struct Jobject{
    jobject native_handler_global;
} Jobject;
JavaVM *jvm = NULL;
Interface *interface = NULL;
// PUBLIC JINTERFACES :
//      0 : onInterface
//      1 : onCaster
//      2 : onProtocol

void onProtocol(struct Interface *interface , ProtocolType type , void *data , char *token , Kmap *request , Kmap *response){
    // global bundles : [ message receiver ] , [ app restart , app upgrade , app online , app offline ]
    if(interface == NULL){
        return;
    }
    JNIEnv *env = NULL;
    if(jvm == NULL){
        return;
    }
    if((*jvm)->AttachCurrentThread(jvm,&env,NULL) != JNI_OK){
        return;
    }
    if(type == PROTOCOL_MESSAGE_SET || type == PROTOCOL_MESSAGE_HANDLE){
        Jobject *jinterface = data;
        if(jinterface != NULL && jinterface->native_handler_global != NULL){
            jclass native_interface_class = (*env)->GetObjectClass(env,jinterface->native_handler_global);
            jmethodID native_interface_method = (*env)->GetMethodID(env,native_interface_class,"onMessage","(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V");

            char *token_string = token == NULL ? kmemory_copy_string("") : kmemory_copy_string(token);
            char *request_string = request == NULL ? kmemory_copy_string("") : kson_pack(request);
            char *response_string = response == NULL ? kmemory_copy_string("") : kson_pack(response);
            jstring native_interface_token = (*env)->NewStringUTF(env,token_string);
            jstring native_interface_request = (*env)->NewStringUTF(env,request_string);
            jstring native_interface_response = (*env)->NewStringUTF(env,response_string);
            kmemory_free(token_string);
            kmemory_free(request_string);
            kmemory_free(response_string);

            (*env)->CallVoidMethod(env,jinterface->native_handler_global,native_interface_method,native_interface_token,native_interface_request,native_interface_response);

            (*env)->DeleteLocalRef(env,native_interface_token);
            (*env)->DeleteLocalRef(env,native_interface_request);
            (*env)->DeleteLocalRef(env,native_interface_response);

            (*env)->DeleteLocalRef(env,native_interface_class);
            if(request != NULL && response != NULL){
                (*env)->DeleteGlobalRef(env,jinterface->native_handler_global);
            }
        }
        jinterface = interface->bundles[2];
        if(jinterface != NULL && jinterface->native_handler_global != NULL){
            jclass native_interface_class = (*env)->GetObjectClass(env,jinterface->native_handler_global);
            jmethodID native_interface_method = (*env)->GetMethodID(env,native_interface_class,"onMessage","(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V");

            char *token_string = token == NULL ? kmemory_copy_string("") : kmemory_copy_string(token);
            char *request_string = request == NULL ? kmemory_copy_string("") : kson_pack(request);
            char *response_string = response == NULL ? kmemory_copy_string("") : kson_pack(response);
            jstring native_interface_token = (*env)->NewStringUTF(env,token_string);
            jstring native_interface_request = (*env)->NewStringUTF(env,request_string);
            jstring native_interface_response = (*env)->NewStringUTF(env,response_string);
            kmemory_free(token_string);
            kmemory_free(request_string);
            kmemory_free(response_string);

            (*env)->CallVoidMethod(env,jinterface->native_handler_global,native_interface_method,native_interface_token,native_interface_request,native_interface_response);

            (*env)->DeleteLocalRef(env,native_interface_token);
            (*env)->DeleteLocalRef(env,native_interface_request);
            (*env)->DeleteLocalRef(env,native_interface_response);

            (*env)->DeleteLocalRef(env,native_interface_class);
        }
    }else{
        Jobject *jinterface = data;
        if(jinterface == NULL || jinterface->native_handler_global == NULL){
            return;
        }
        jclass native_interface_class = (*env)->GetObjectClass(env,jinterface->native_handler_global);
        jmethodID native_interface_method = (*env)->GetMethodID(env,native_interface_class,"onMessage","(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V");

        char *token_string = token == NULL ? kmemory_copy_string("") : kmemory_copy_string(token);
        char *request_string = request == NULL ? kmemory_copy_string("") : kson_pack(request);
        char *response_string = response == NULL ? kmemory_copy_string("") : kson_pack(response);
        jstring native_interface_token = (*env)->NewStringUTF(env,token_string);
        jstring native_interface_request = (*env)->NewStringUTF(env,request_string);
        jstring native_interface_response = (*env)->NewStringUTF(env,response_string);
        kmemory_free(token_string);
        kmemory_free(request_string);
        kmemory_free(response_string);

        (*env)->CallVoidMethod(env,jinterface->native_handler_global,native_interface_method,native_interface_token,native_interface_request,native_interface_response);

        (*env)->DeleteLocalRef(env,native_interface_token);
        (*env)->DeleteLocalRef(env,native_interface_request);
        (*env)->DeleteLocalRef(env,native_interface_response);

        (*env)->DeleteLocalRef(env,native_interface_class);
        if(request != NULL && response != NULL){
            if(type == PROTOCOL_PEER_LOAD){
                if(response->indexof(response,"VALUE") < 0){
                    (*env)->DeleteGlobalRef(env,jinterface->native_handler_global);
                }
            }else{
                (*env)->DeleteGlobalRef(env,jinterface->native_handler_global);
            }
        }
    }
}
void onStream(struct Interface *interface , StreamType type , void *data , char *link , long seek){
    if(interface == NULL){
        return;
    }
    JNIEnv *env = NULL;
    if(jvm == NULL){
        return;
    }
    if((*jvm)->AttachCurrentThread(jvm,&env,NULL) != JNI_OK){
        return;
    }
    Jobject *jinterface = data;
    if(jinterface != NULL && jinterface->native_handler_global != NULL){
        jclass native_interface_class = (*env)->GetObjectClass(env,jinterface->native_handler_global);
        jmethodID native_interface_method;
        switch (type){
            case STREAM_START:
                native_interface_method = (*env)->GetMethodID(env,native_interface_class,"onStart","(Ljava/lang/String;)V");
                break;
            case STREAM_OPEN:
                native_interface_method = (*env)->GetMethodID(env,native_interface_class,"onOpen","(Ljava/lang/String;)V");
                break;
            case STREAM_HANDSHAKE:
                native_interface_method = (*env)->GetMethodID(env,native_interface_class,"onHandshake","(Ljava/lang/String;)V");
                break;
            case STREAM_DATA:
                native_interface_method = (*env)->GetMethodID(env,native_interface_class,"onData","(Ljava/lang/String;J)V");
                break;
            default:
                native_interface_method = (*env)->GetMethodID(env,native_interface_class,"onStop","(Ljava/lang/String;)V");
                break;
        }
        char *link_string = link == NULL ? kmemory_copy_string("") : kmemory_copy_string(link);
        jstring native_interface_link = (*env)->NewStringUTF(env,link_string);
        kmemory_free(link_string);

        if(type == STREAM_DATA){
            (*env)->CallVoidMethod(env,jinterface->native_handler_global,native_interface_method,native_interface_link,seek);
        }else{
            (*env)->CallVoidMethod(env,jinterface->native_handler_global,native_interface_method,native_interface_link);
        }

        (*env)->DeleteLocalRef(env,native_interface_link);

        (*env)->DeleteLocalRef(env,native_interface_class);
        if(type == STREAM_STOP){
            (*env)->DeleteGlobalRef(env,jinterface->native_handler_global);
        }
    }
}
void onCaster(struct Interface *interface , CasterType type , char *target_id , void *data , int size , CasterTune tune){
    if(interface == NULL){
        return;
    }
    JNIEnv *env = NULL;
    if(jvm == NULL){
        return;
    }
    if((*jvm)->AttachCurrentThread(jvm,&env,NULL) != JNI_OK){
        return;
    }
    Jobject *jinterface = interface->bundles[1];
    if(jinterface != NULL && jinterface->native_handler_global != NULL){
        jclass native_interface_class = (*env)->GetObjectClass(env,jinterface->native_handler_global);
        jmethodID native_interface_method;
        switch (type){
            case CASTER_TUNE:
                native_interface_method = (*env)->GetMethodID(env,native_interface_class,"onTune","(Ljava/lang/String;I)V");
                break;
            default:
                native_interface_method = (*env)->GetMethodID(env,native_interface_class,"onCast","(Ljava/lang/String;[BI)V");
                break;
        }
        char *target_id_string = link == NULL ? kmemory_copy_string("") : kmemory_copy_string(target_id);
        jstring native_interface_link = (*env)->NewStringUTF(env,target_id_string);
        kmemory_free(target_id_string);
        if(type == CASTER_TUNE){
            int caster_tune;
            switch (tune){
                case CASTER_TUNE_OPEN:
                    caster_tune = 0;
                    break;
                case CASTER_TUNE_RDWR:
                    caster_tune = 1;
                    break;
                case CASTER_TUNE_RDONLY:
                    caster_tune = 2;
                    break;
                case CASTER_TUNE_WRONLY:
                    caster_tune = 3;
                    break;
                case CASTER_TUNE_NONE:
                    caster_tune = 4;
                    break;
                case CASTER_TUNE_WAIT:
                    caster_tune = 5;
                    break;
                case CASTER_TUNE_CLOSE:
                    caster_tune = 6;
                    break;
                default:
                    caster_tune = 7;
                    break;
            }
            (*env)->CallVoidMethod(env,jinterface->native_handler_global,native_interface_method,native_interface_link,caster_tune);
        }else{
            jbyteArray native_interface_data = (*env)->NewByteArray(env,size);
            (*env)->SetByteArrayRegion(env,native_interface_data,0,size,data);
            (*env)->CallVoidMethod(env,jinterface->native_handler_global,native_interface_method,native_interface_link,native_interface_data,size);
            (*env)->ReleaseByteArrayElements(env,native_interface_data,data,0);
        }
        (*env)->DeleteLocalRef(env,native_interface_link);

        (*env)->DeleteLocalRef(env,native_interface_class);
        if(type == STREAM_STOP){
            (*env)->DeleteGlobalRef(env,jinterface->native_handler_global);
        }
    }
}
void onInterface(struct Interface *interface , InterfaceType type , Kmap *message){
    if(interface == NULL){
        return;
    }
    JNIEnv *env = NULL;
    if(jvm == NULL){
        return;
    }
    if((*jvm)->AttachCurrentThread(jvm,&env,NULL) != JNI_OK){
        return;
    }
    Jobject *jinterface = interface->bundles[0];
    if(jinterface != NULL && jinterface->native_handler_global != NULL){
        jclass native_interface_class = (*env)->GetObjectClass(env,jinterface->native_handler_global);
        if(type == INTERFACE_APPRESTART){
            jmethodID native_interface_method = (*env)->GetMethodID(env,native_interface_class,"onAppRestart","()V");
            (*env)->CallVoidMethod(env,jinterface->native_handler_global,native_interface_method);
        }else if(type == INTERFACE_APPUPGRADE){
            jmethodID native_interface_method = (*env)->GetMethodID(env,native_interface_class,"onAppUpgrade","(Ljava/lang/String;)V");
            char *message_string = message == NULL ? kmemory_copy_string("") : kson_pack(message);
            jstring native_interface_message = (*env)->NewStringUTF(env,message_string);
            kmemory_free(message_string);
            (*env)->CallVoidMethod(env,jinterface->native_handler_global,native_interface_method,native_interface_message);
            (*env)->DeleteLocalRef(env,native_interface_message);
        }else if(type == INTERFACE_APPHANDSHAKE){
            jmethodID native_interface_method = (*env)->GetMethodID(env,native_interface_class,"onAppHandshake","()V");
            (*env)->CallVoidMethod(env,jinterface->native_handler_global,native_interface_method);
        }else if(type == INTERFACE_APPONLINE){
            jmethodID native_interface_method = (*env)->GetMethodID(env,native_interface_class,"onAppOnline","()V");
            (*env)->CallVoidMethod(env,jinterface->native_handler_global,native_interface_method);
        }else if(type == INTERFACE_APPOFFLINE){
            jmethodID native_interface_method = (*env)->GetMethodID(env,native_interface_class,"onAppOffline","()V");
            (*env)->CallVoidMethod(env,jinterface->native_handler_global,native_interface_method);
        }else if(type == INTERFACE_STREAMSET){
            jmethodID native_interface_method = (*env)->GetMethodID(env,native_interface_class,"onStreamSet","(Ljava/lang/String;)V");
            char *message_string = message == NULL ? kmemory_copy_string("") : kson_pack(message);
            jstring native_interface_message = (*env)->NewStringUTF(env,message_string);
            kmemory_free(message_string);
            (*env)->CallVoidMethod(env,jinterface->native_handler_global,native_interface_method,native_interface_message);
            (*env)->DeleteLocalRef(env,native_interface_message);
        }else{
            jmethodID native_interface_method = (*env)->GetMethodID(env,native_interface_class,"onStreamGet","(Ljava/lang/String;)V");
            char *message_string = message == NULL ? kmemory_copy_string("") : kson_pack(message);
            jstring native_interface_message = (*env)->NewStringUTF(env,message_string);
            kmemory_free(message_string);
            (*env)->CallVoidMethod(env,jinterface->native_handler_global,native_interface_method,native_interface_message);
            (*env)->DeleteLocalRef(env,native_interface_message);
        }
        (*env)->DeleteLocalRef(env,native_interface_class);
    }
}
void onProtocolError(char *token , void *data , Kmap *request , Kmap *response){
    if(interface == NULL){
        return;
    }
    JNIEnv *env = NULL;
    if(jvm == NULL){
        return;
    }
    if((*jvm)->AttachCurrentThread(jvm,&env,NULL) != JNI_OK){
        return;
    }
    Jobject *jinterface = data;
    if(jinterface == NULL || jinterface->native_handler_global == NULL){
        return;
    }
    jclass native_interface_class = (*env)->GetObjectClass(env,jinterface->native_handler_global);
    jmethodID native_interface_method = (*env)->GetMethodID(env,native_interface_class,"onMessage","(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V");

    char *token_string = token == NULL ? kmemory_copy_string("") : kmemory_copy_string(token);
    char *request_string = request == NULL ? kmemory_copy_string("") : kson_pack(request);
    char *response_string = response == NULL ? kmemory_copy_string("") : kson_pack(response);
    jstring native_interface_token = (*env)->NewStringUTF(env,token_string);
    jstring native_interface_request = (*env)->NewStringUTF(env,request_string);
    jstring native_interface_response = (*env)->NewStringUTF(env,response_string);
    jstring native_interface_null = (*env)->NewStringUTF(env,"");
    kmemory_free(token_string);
    kmemory_free(request_string);
    kmemory_free(response_string);

    (*env)->CallVoidMethod(env,jinterface->native_handler_global,native_interface_method,native_interface_token,native_interface_request,native_interface_null);
    (*env)->CallVoidMethod(env,jinterface->native_handler_global,native_interface_method,native_interface_token,native_interface_request,native_interface_response);

    (*env)->DeleteLocalRef(env,native_interface_token);
    (*env)->DeleteLocalRef(env,native_interface_request);
    (*env)->DeleteLocalRef(env,native_interface_response);
    (*env)->DeleteLocalRef(env,native_interface_null);

    (*env)->DeleteLocalRef(env,native_interface_class);
}
void onStreamError(char *link , void *data){
    if(interface == NULL){
        return;
    }
    JNIEnv *env = NULL;
    if(jvm == NULL){
        return;
    }
    if((*jvm)->AttachCurrentThread(jvm,&env,NULL) != JNI_OK){
        return;
    }
    Jobject *jinterface = data;
    if(jinterface == NULL || jinterface->native_handler_global == NULL){
        return;
    }
    jclass native_interface_class = (*env)->GetObjectClass(env,jinterface->native_handler_global);


    jmethodID native_interface_start = (*env)->GetMethodID(env,native_interface_class,"onStart","(Ljava/lang/String;)V");
    jmethodID native_interface_stop = (*env)->GetMethodID(env,native_interface_class,"onStop","(Ljava/lang/String;)V");

    char *link_string = link == NULL ? kmemory_copy_string("") : kmemory_copy_string(link);
    jstring native_interface_link = (*env)->NewStringUTF(env,link_string);
    kmemory_free(link_string);

    (*env)->CallVoidMethod(env,jinterface->native_handler_global,native_interface_start,native_interface_link);
    (*env)->CallVoidMethod(env,jinterface->native_handler_global,native_interface_stop,native_interface_link);

    (*env)->DeleteLocalRef(env,native_interface_link);

    (*env)->DeleteLocalRef(env,native_interface_class);
}
void destructor(void *old_data){
    JNIEnv *env = NULL;
    if(jvm == NULL){
        return;
    }
    if((*jvm)->AttachCurrentThread(jvm,&env,NULL) != JNI_OK){
        return;
    }
    Jobject *jinterface = old_data;
    if(jinterface == NULL || jinterface->native_handler_global == NULL){
        return;
    }
    (*env)->DeleteGlobalRef(env,jinterface->native_handler_global);
    kmemory_free(jinterface);
}

JNIEXPORT void JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_interface_1new(JNIEnv *env, jobject instance,
                                                                               jstring protocol_host_,
                                                                               jint protocol_port,
                                                                               jint stream_parallelism,
                                                                               jstring stream_host_,
                                                                               jint stream_port,
                                                                               jint stream_pool,
                                                                               jstring caster_host_,
                                                                               jint caster_port,
                                                                               jint caster_pool,
                                                                               jstring interface_path_,
                                                                               jstring self_id_) {
    if(interface != NULL){
        return;
    }
    const char *protocol_host = (*env)->GetStringUTFChars(env, protocol_host_, 0);
    const char *stream_host = (*env)->GetStringUTFChars(env, stream_host_, 0);
    const char *caster_host = (*env)->GetStringUTFChars(env, caster_host_, 0);
    const char *interface_path = (*env)->GetStringUTFChars(env, interface_path_, 0);
    const char *self_id = (*env)->GetStringUTFChars(env, self_id_, 0);
    if((*env)->GetJavaVM(env, &jvm) != JNI_OK){
        jvm = NULL;
        return;
    }
    if(interface == NULL){
        interface = interface_new(
                (char *) protocol_host,
                protocol_port,
                stream_parallelism ? STREAM_PARALLELISM_PARALLEL : STREAM_PARALLELISM_SEQUENTIAL,
                (char *) stream_host,
                stream_port,
                stream_pool,
                (char *) caster_host,
                caster_port,
                caster_pool,
                (char *) interface_path,
                (char *) self_id,
                4,
                onProtocol,
                onStream,
                onCaster,
                onInterface
        );
    }
    (*env)->ReleaseStringUTFChars(env, protocol_host_, protocol_host);
    (*env)->ReleaseStringUTFChars(env, stream_host_, stream_host);
    (*env)->ReleaseStringUTFChars(env, caster_host_, caster_host);
    (*env)->ReleaseStringUTFChars(env, interface_path_, interface_path);
    (*env)->ReleaseStringUTFChars(env, self_id_, self_id);
}

JNIEXPORT void JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_interface_1free(JNIEnv *env, jobject instance) {
    if(interface == NULL){
        return;
    }
//    interface_free(interface);
    interface = NULL;
}

JNIEXPORT jint JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_interface_1start(JNIEnv *env, jobject instance) {
    if(interface == NULL){
        return 0;
    }
    return interface->start(interface);
}

JNIEXPORT jint JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_interface_1state(JNIEnv *env, jobject instance) {
    if(interface == NULL){
        return 0;
    }
    InterfaceState state = interface->state(interface);
    switch (state){
        case INTERFACE_STATE_STOP:
            return 0;
        case INTERFACE_STATE_START:
            return 1;
        case INTERFACE_STATE_OFFLINE:
            return 2;
        default:
            return 3;
    }
}

JNIEXPORT jint JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_interface_1stop(JNIEnv *env, jobject instance) {
    if(interface == NULL){
        return 0;
    }
    return interface->stop(interface);
}

JNIEXPORT jint JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_interface_1bundle(JNIEnv *env, jobject instance,
                                                                                  jint index,
                                                                                  jobject interface_interface) {
    if(interface == NULL){
        return 0;
    }
    Jobject *jinterface = kmemory_alloc(sizeof(Jobject));
    jinterface->native_handler_global = (*env)->NewGlobalRef(env,interface_interface);
    if(interface->bundles[index] != NULL){
        destructor(interface->bundles[index]);
    }
    interface->bundles[index] = jinterface;
    return 1;
}

JNIEXPORT jint JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_interface_1self_1set(JNIEnv *env, jobject instance,
                                                                                     jstring key_,
                                                                                     jstring value_) {
    if(interface == NULL){
        return 0;
    }
    const char *key = (*env)->GetStringUTFChars(env, key_, 0);
    const char *value = (*env)->GetStringUTFChars(env, value_, 0);
    int result = interface->self_set(interface, (char *) key, (char *) value);
    (*env)->ReleaseStringUTFChars(env, key_, key);
    (*env)->ReleaseStringUTFChars(env, value_, value);
    return result;
}

JNIEXPORT jstring JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_interface_1self_1get(JNIEnv *env, jobject instance,
                                                                                     jstring key_) {
    if(interface == NULL){
        return NULL;
    }
    const char *key = (*env)->GetStringUTFChars(env, key_, 0);
    char *result = interface->self_get(interface, (char *) key);
    (*env)->ReleaseStringUTFChars(env, key_, key);
    return (*env)->NewStringUTF(env, result == NULL ? "" : result);
}

JNIEXPORT jint JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_protocol_1bundle(JNIEnv *env, jobject instance,
                                                                                 jint index,
                                                                                 jobject protocol_interface) {
    if(interface == NULL){
        return 0;
    }
    Jobject *jinterface = kmemory_alloc(sizeof(Jobject));
    jinterface->native_handler_global = (*env)->NewGlobalRef(env,protocol_interface);
    if(interface->bundles[index] != NULL){
        destructor(interface->bundles[index]);
    }
    interface->bundles[index] = jinterface;
    return 1;
}

JNIEXPORT jint JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_protocol_1update(JNIEnv *env, jobject instance,
                                                                                 jstring token_,
                                                                                 jobject protocol_interface) {
    if(interface == NULL){
        return 0;
    }
    const char *token = (*env)->GetStringUTFChars(env, token_, 0);
    Jobject *jinterface = kmemory_alloc(sizeof(Jobject));
    jinterface->native_handler_global = (*env)->NewGlobalRef(env,protocol_interface);
    int result = interface->protocol_update(interface, jinterface, (char *) token, destructor);
    (*env)->ReleaseStringUTFChars(env, token_, token);
    return result;
}

JNIEXPORT void JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_protocol_1peer_1captcha(JNIEnv *env, jobject instance,
                                                                                        jobject protocol_interface,
                                                                                        jstring token_,
                                                                                        jstring phone_,
                                                                                        jstring type_) {
    if(interface == NULL){
        return;
    }
    const char *token = token_ == NULL ? NULL : (*env)->GetStringUTFChars(env, token_, 0);
    const char *phone = phone_ == NULL ? NULL : (*env)->GetStringUTFChars(env, phone_, 0);
    const char *type = type_ == NULL ? NULL : (*env)->GetStringUTFChars(env, type_, 0);
    Jobject *jinterface = kmemory_alloc(sizeof(Jobject));
    jinterface->native_handler_global = (*env)->NewGlobalRef(env,protocol_interface);
    if(!interface->protocol_peer_captcha(interface, jinterface, (char *) token, (char *) phone, (char *) type)){
        Kmap *request = interface->protocol_request(interface,PROTOCOL_PEER_CAPTCHA,token,phone,type);
        Kmap *response = kson_parse("{}");
        response->put(response,"DATA_TYPE",KTYPE_STACK,request->get(request,"DATA_TYPE"),KTYPE_STACK);
        response->put(response,"TOKEN",KTYPE_STACK,request->get(request,"TOKEN"),KTYPE_STACK);
        response->put(response,"ERROR",KTYPE_STACK,"OFF",KTYPE_STACK);
        onProtocolError((char *) token, jinterface, request, response);
        destructor(jinterface);
        kmap_free(request);
        kmap_free(response);
    }
    if(token_ != NULL){
        (*env)->ReleaseStringUTFChars(env, token_, token);
    }
    if(phone_ != NULL){
        (*env)->ReleaseStringUTFChars(env, phone_, phone);
    }
    if(type_ != NULL){
        (*env)->ReleaseStringUTFChars(env, type_, type);
    }
}

JNIEXPORT void JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_protocol_1peer_1login(JNIEnv *env, jobject instance,
                                                                                      jobject protocol_interface,
                                                                                      jstring token_,
                                                                                      jstring id_,
                                                                                      jstring scode_,
                                                                                      jstring platform_,
                                                                                      jstring version_) {
    if(interface == NULL){
        return;
    }
    const char *token = token_ == NULL ? NULL : (*env)->GetStringUTFChars(env, token_, 0);
    const char *id = id_ == NULL ? NULL : (*env)->GetStringUTFChars(env, id_, 0);
    const char *scode = scode_ == NULL ? NULL : (*env)->GetStringUTFChars(env, scode_, 0);
    const char *platform = platform_ == NULL ? NULL : (*env)->GetStringUTFChars(env, platform_, 0);
    const char *version = version_ == NULL ? NULL : (*env)->GetStringUTFChars(env, version_, 0);
    Jobject *jinterface = kmemory_alloc(sizeof(Jobject));
    jinterface->native_handler_global = (*env)->NewGlobalRef(env,protocol_interface);
    if(!interface->protocol_peer_login(interface, jinterface, (char *) token, (char *) id, (char *) scode, (char *) platform, (char *) version)){
        Kmap *request = interface->protocol_request(interface,PROTOCOL_PEER_LOGIN,token,id,scode,platform,version);
        Kmap *response = kson_parse("{}");
        response->put(response,"DATA_TYPE",KTYPE_STACK,request->get(request,"DATA_TYPE"),KTYPE_STACK);
        response->put(response,"TOKEN",KTYPE_STACK,request->get(request,"TOKEN"),KTYPE_STACK);
        response->put(response,"ERROR",KTYPE_STACK,"OFF",KTYPE_STACK);
        onProtocolError((char *) token, jinterface, request, response);
        destructor(jinterface);
        kmap_free(request);
        kmap_free(response);
    }
    if(token_ != NULL){
        (*env)->ReleaseStringUTFChars(env, token_, token);
    }
    if(id_ != NULL){
        (*env)->ReleaseStringUTFChars(env, id_, id);
    }
    if(scode_ != NULL){
        (*env)->ReleaseStringUTFChars(env, scode_, scode);
    }
    if(platform_ != NULL){
        (*env)->ReleaseStringUTFChars(env, platform_, platform);
    }
    if(version_ != NULL){
        (*env)->ReleaseStringUTFChars(env, version_, version);
    }
}

JNIEXPORT void JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_protocol_1peer_1logout(JNIEnv *env, jobject instance,
                                                                                       jobject protocol_interface,
                                                                                       jstring token_) {
    if(interface == NULL){
        return;
    }
    const char *token = token_ == NULL ? NULL : (*env)->GetStringUTFChars(env, token_, 0);
    Jobject *jinterface = kmemory_alloc(sizeof(Jobject));
    jinterface->native_handler_global = (*env)->NewGlobalRef(env,protocol_interface);
    if(!interface->protocol_peer_logout(interface, jinterface, (char *) token)){
        Kmap *request = interface->protocol_request(interface,PROTOCOL_PEER_LOGOUT,token);
        Kmap *response = kson_parse("{}");
        response->put(response,"DATA_TYPE",KTYPE_STACK,request->get(request,"DATA_TYPE"),KTYPE_STACK);
        response->put(response,"TOKEN",KTYPE_STACK,request->get(request,"TOKEN"),KTYPE_STACK);
        response->put(response,"ERROR",KTYPE_STACK,"OFF",KTYPE_STACK);
        onProtocolError((char *) token, jinterface, request, response);
        destructor(jinterface);
        kmap_free(request);
        kmap_free(response);
    }
    if(token_ != NULL){
        (*env)->ReleaseStringUTFChars(env, token_, token);
    }
}

JNIEXPORT void JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_protocol_1peer_1create(JNIEnv *env, jobject instance,
                                                                                       jobject protocol_interface,
                                                                                       jstring token_,
                                                                                       jstring phone_,
                                                                                       jstring captcha_) {
    if(interface == NULL){
        return;
    }
    const char *token = token_ == NULL ? NULL : (*env)->GetStringUTFChars(env, token_, 0);
    const char *phone = phone_ == NULL ? NULL : (*env)->GetStringUTFChars(env, phone_, 0);
    const char *captcha = captcha_ == NULL ? NULL : (*env)->GetStringUTFChars(env, captcha_, 0);
    Jobject *jinterface = kmemory_alloc(sizeof(Jobject));
    jinterface->native_handler_global = (*env)->NewGlobalRef(env,protocol_interface);
    if(!interface->protocol_peer_create(interface, jinterface, (char *) token, (char *) phone, (char *) captcha)){
        Kmap *request = interface->protocol_request(interface,PROTOCOL_PEER_CREATE,token,phone,captcha);
        Kmap *response = kson_parse("{}");
        response->put(response,"DATA_TYPE",KTYPE_STACK,request->get(request,"DATA_TYPE"),KTYPE_STACK);
        response->put(response,"TOKEN",KTYPE_STACK,request->get(request,"TOKEN"),KTYPE_STACK);
        response->put(response,"ERROR",KTYPE_STACK,"OFF",KTYPE_STACK);
        onProtocolError((char *) token, jinterface, request, response);
        destructor(jinterface);
        kmap_free(request);
        kmap_free(response);
    }
    if(token_ != NULL){
        (*env)->ReleaseStringUTFChars(env, token_, token);
    }
    if(phone_ != NULL){
        (*env)->ReleaseStringUTFChars(env, phone_, phone);
    }
    if(captcha_ != NULL){
        (*env)->ReleaseStringUTFChars(env, captcha_, captcha);
    }
}

JNIEXPORT void JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_protocol_1peer_1delete(JNIEnv *env, jobject instance,
                                                                                       jobject protocol_interface,
                                                                                       jstring token_) {
    if(interface == NULL){
        return;
    }
    const char *token = token_ == NULL ? NULL : (*env)->GetStringUTFChars(env, token_, 0);
    Jobject *jinterface = kmemory_alloc(sizeof(Jobject));
    jinterface->native_handler_global = (*env)->NewGlobalRef(env,protocol_interface);
    if(!interface->protocol_peer_delete(interface, jinterface, (char *) token)){
        Kmap *request = interface->protocol_request(interface,PROTOCOL_PEER_DELETE,token);
        Kmap *response = kson_parse("{}");
        response->put(response,"DATA_TYPE",KTYPE_STACK,request->get(request,"DATA_TYPE"),KTYPE_STACK);
        response->put(response,"TOKEN",KTYPE_STACK,request->get(request,"TOKEN"),KTYPE_STACK);
        response->put(response,"ERROR",KTYPE_STACK,"OFF",KTYPE_STACK);
        onProtocolError((char *) token, jinterface, request, response);
        destructor(jinterface);
        kmap_free(request);
        kmap_free(response);
    }
    if(token_ != NULL){
        (*env)->ReleaseStringUTFChars(env, token_, token);
    }
}

JNIEXPORT void JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_protocol_1peer_1load(JNIEnv *env, jobject instance,
                                                                                     jobject protocol_interface,
                                                                                     jstring token_,
                                                                                     jstring id_,
                                                                                     jstring type_,
                                                                                     jstring mode_,
                                                                                     jstring param_1_,
                                                                                     jstring param_2_,
                                                                                     jint local,
                                                                                     jint trying) {
    if(interface == NULL){
        return;
    }
    const char *token = token_ == NULL ? NULL : (*env)->GetStringUTFChars(env, token_, 0);
    const char *id = id_ == NULL ? NULL : (*env)->GetStringUTFChars(env, id_, 0);
    const char *type = type_ == NULL ? NULL : (*env)->GetStringUTFChars(env, type_, 0);
    const char *mode = mode_ == NULL ? NULL : (*env)->GetStringUTFChars(env, mode_, 0);
    const char *param_1 = param_1_ == NULL ? NULL : (*env)->GetStringUTFChars(env, param_1_, 0);
    const char *param_2 = param_2_ == NULL ? NULL : (*env)->GetStringUTFChars(env, param_2_, 0);
    Jobject *jinterface = kmemory_alloc(sizeof(Jobject));
    jinterface->native_handler_global = (*env)->NewGlobalRef(env,protocol_interface);
    if(!interface->protocol_peer_load(interface, jinterface, (char *) token, (char *) id, (char *) type, (char *) mode, (char *) param_1, (char *) param_2, local, trying)){
        Kmap *request = interface->protocol_request(interface,PROTOCOL_PEER_LOAD,token,id,type,mode,param_1,param_2);
        Kmap *response = kson_parse("{}");
        response->put(response,"DATA_TYPE",KTYPE_STACK,request->get(request,"DATA_TYPE"),KTYPE_STACK);
        response->put(response,"TOKEN",KTYPE_STACK,request->get(request,"TOKEN"),KTYPE_STACK);
        response->put(response,"ERROR",KTYPE_STACK,"OFF",KTYPE_STACK);
        onProtocolError((char *) token, jinterface, request, response);
        destructor(jinterface);
        kmap_free(request);
        kmap_free(response);
    }
    if(token_ != NULL){
        (*env)->ReleaseStringUTFChars(env, token_, token);
    }
    if(id_ != NULL){
        (*env)->ReleaseStringUTFChars(env, id_, id);
    }
    if(type_ != NULL){
        (*env)->ReleaseStringUTFChars(env, type_, type);
    }
    if(mode_ != NULL){
        (*env)->ReleaseStringUTFChars(env, mode_, mode);
    }
    if(param_1_ != NULL){
        (*env)->ReleaseStringUTFChars(env, param_1_, param_1);
    }
    if(param_2_ != NULL){
        (*env)->ReleaseStringUTFChars(env, param_2_, param_2);
    }
}

JNIEXPORT void JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_protocol_1peer_1set(JNIEnv *env, jobject instance,
                                                                                    jobject protocol_interface,
                                                                                    jstring token_,
                                                                                    jstring id_,
                                                                                    jstring key_,
                                                                                    jstring option_,
                                                                                    jstring value_,
                                                                                    jstring operator_) {
    if(interface == NULL){
        return;
    }
    const char *token = token_ == NULL ? NULL : (*env)->GetStringUTFChars(env, token_, 0);
    const char *id = id_ == NULL ? NULL : (*env)->GetStringUTFChars(env, id_, 0);
    const char *key = key_ == NULL ? NULL : (*env)->GetStringUTFChars(env, key_, 0);
    const char *option = option_ == NULL ? NULL : (*env)->GetStringUTFChars(env, option_, 0);
    const char *value = value_ == NULL ? NULL : (*env)->GetStringUTFChars(env, value_, 0);
    const char *operator = operator_ == NULL ? NULL : (*env)->GetStringUTFChars(env, operator_, 0);
    Jobject *jinterface = kmemory_alloc(sizeof(Jobject));
    jinterface->native_handler_global = (*env)->NewGlobalRef(env,protocol_interface);
    if(!interface->protocol_peer_set(interface, jinterface, (char *) token, (char *) id, (char *) key, (char *) option, (char *) value, (char *) operator)){
        Kmap *request = interface->protocol_request(interface,PROTOCOL_PEER_SET,token,id,key,option,value,operator);
        Kmap *response = kson_parse("{}");
        response->put(response,"DATA_TYPE",KTYPE_STACK,request->get(request,"DATA_TYPE"),KTYPE_STACK);
        response->put(response,"TOKEN",KTYPE_STACK,request->get(request,"TOKEN"),KTYPE_STACK);
        response->put(response,"ERROR",KTYPE_STACK,"OFF",KTYPE_STACK);
        onProtocolError((char *) token, jinterface, request, response);
        destructor(jinterface);
        kmap_free(request);
        kmap_free(response);
    }
    if(token_ != NULL){
        (*env)->ReleaseStringUTFChars(env, token_, token);
    }
    if(id_ != NULL){
        (*env)->ReleaseStringUTFChars(env, id_, id);
    }
    if(key_ != NULL){
        (*env)->ReleaseStringUTFChars(env, key_, key);
    }
    if(option_ != NULL){
        (*env)->ReleaseStringUTFChars(env, option_, option);
    }
    if(value_ != NULL){
        (*env)->ReleaseStringUTFChars(env, value_, value);
    }
    if(operator_ != NULL){
        (*env)->ReleaseStringUTFChars(env, operator_, operator);
    }
}

JNIEXPORT void JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_protocol_1peer_1id(JNIEnv *env, jobject instance,
                                                                                   jobject protocol_interface,
                                                                                   jstring token_,
                                                                                   jstring key_,
                                                                                   jstring type_) {
    if(interface == NULL){
        return;
    }
    const char *token = token_ == NULL ? NULL : (*env)->GetStringUTFChars(env, token_, 0);
    const char *key = key_ == NULL ? NULL : (*env)->GetStringUTFChars(env, key_, 0);
    const char *type = type_ == NULL ? NULL : (*env)->GetStringUTFChars(env, type_, 0);
    Jobject *jinterface = kmemory_alloc(sizeof(Jobject));
    jinterface->native_handler_global = (*env)->NewGlobalRef(env,protocol_interface);
    if(!interface->protocol_peer_id(interface, jinterface, (char *) token, (char *) key, (char *) type)){
        Kmap *request = interface->protocol_request(interface,PROTOCOL_PEER_ID,token,key,type);
        Kmap *response = kson_parse("{}");
        response->put(response,"DATA_TYPE",KTYPE_STACK,request->get(request,"DATA_TYPE"),KTYPE_STACK);
        response->put(response,"TOKEN",KTYPE_STACK,request->get(request,"TOKEN"),KTYPE_STACK);
        response->put(response,"ERROR",KTYPE_STACK,"OFF",KTYPE_STACK);
        onProtocolError((char *) token, jinterface, request, response);
        destructor(jinterface);
        kmap_free(request);
        kmap_free(response);
    }
    if(token_ != NULL){
        (*env)->ReleaseStringUTFChars(env, token_, token);
    }
    if(key_ != NULL){
        (*env)->ReleaseStringUTFChars(env, key_, key);
    }
    if(type_ != NULL){
        (*env)->ReleaseStringUTFChars(env, type_, type);
    }
}

JNIEXPORT void JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_protocol_1connection_1create(JNIEnv *env, jobject instance,
                                                                                             jobject protocol_interface,
                                                                                             jstring token_) {
    if(interface == NULL){
        return;
    }
    const char *token = token_ == NULL ? NULL : (*env)->GetStringUTFChars(env, token_, 0);
    Jobject *jinterface = kmemory_alloc(sizeof(Jobject));
    jinterface->native_handler_global = (*env)->NewGlobalRef(env,protocol_interface);
    if(!interface->protocol_connection_create(interface, jinterface, (char *) token)){
        Kmap *request = interface->protocol_request(interface,PROTOCOL_CONNECTION_CREATE,token);
        Kmap *response = kson_parse("{}");
        response->put(response,"DATA_TYPE",KTYPE_STACK,request->get(request,"DATA_TYPE"),KTYPE_STACK);
        response->put(response,"TOKEN",KTYPE_STACK,request->get(request,"TOKEN"),KTYPE_STACK);
        response->put(response,"ERROR",KTYPE_STACK,"OFF",KTYPE_STACK);
        onProtocolError((char *) token, jinterface, request, response);
        destructor(jinterface);
        kmap_free(request);
        kmap_free(response);
    }
    if(token_ != NULL){
        (*env)->ReleaseStringUTFChars(env, token_, token);
    }
}

JNIEXPORT void JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_protocol_1connection_1delete(JNIEnv *env, jobject instance,
                                                                                             jobject protocol_interface,
                                                                                             jstring token_,
                                                                                             jstring id_) {
    if(interface == NULL){
        return;
    }
    const char *token = token_ == NULL ? NULL : (*env)->GetStringUTFChars(env, token_, 0);
    const char *id = id_ == NULL ? NULL : (*env)->GetStringUTFChars(env, id_, 0);
    Jobject *jinterface = kmemory_alloc(sizeof(Jobject));
    jinterface->native_handler_global = (*env)->NewGlobalRef(env,protocol_interface);
    if(!interface->protocol_connection_delete(interface, jinterface, (char *) token, (char *) id)){
        Kmap *request = interface->protocol_request(interface,PROTOCOL_CONNECTION_DELETE,token,id);
        Kmap *response = kson_parse("{}");
        response->put(response,"DATA_TYPE",KTYPE_STACK,request->get(request,"DATA_TYPE"),KTYPE_STACK);
        response->put(response,"TOKEN",KTYPE_STACK,request->get(request,"TOKEN"),KTYPE_STACK);
        response->put(response,"ERROR",KTYPE_STACK,"OFF",KTYPE_STACK);
        onProtocolError((char *) token, jinterface, request, response);
        destructor(jinterface);
        kmap_free(request);
        kmap_free(response);
    }
    if(token_ != NULL){
        (*env)->ReleaseStringUTFChars(env, token_, token);
    }
    if(id_ != NULL){
        (*env)->ReleaseStringUTFChars(env, id_, id);
    }
}

JNIEXPORT void JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_protocol_1connection_1set(JNIEnv *env, jobject instance,
                                                                                          jobject protocol_interface,
                                                                                          jstring token_,
                                                                                          jstring id_,
                                                                                          jstring key_,
                                                                                          jstring option_,
                                                                                          jstring value_,
                                                                                          jstring operator_) {
    if(interface == NULL){
        return;
    }
    const char *token = token_ == NULL ? NULL : (*env)->GetStringUTFChars(env, token_, 0);
    const char *id = id_ == NULL ? NULL : (*env)->GetStringUTFChars(env, id_, 0);
    const char *key = key_ == NULL ? NULL : (*env)->GetStringUTFChars(env, key_, 0);
    const char *option = option_ == NULL ? NULL : (*env)->GetStringUTFChars(env, option_, 0);
    const char *value = value_ == NULL ? NULL : (*env)->GetStringUTFChars(env, value_, 0);
    const char *operator = operator_ == NULL ? NULL : (*env)->GetStringUTFChars(env, operator_, 0);
    Jobject *jinterface = kmemory_alloc(sizeof(Jobject));
    jinterface->native_handler_global = (*env)->NewGlobalRef(env,protocol_interface);
    if(!interface->protocol_connection_set(interface, jinterface, (char *) token, (char *) id, (char *) key, (char *) option, (char *) value, (char *) operator)){
        Kmap *request = interface->protocol_request(interface,PROTOCOL_CONNECTION_SET,token,id,key,option,value,operator);
        Kmap *response = kson_parse("{}");
        response->put(response,"DATA_TYPE",KTYPE_STACK,request->get(request,"DATA_TYPE"),KTYPE_STACK);
        response->put(response,"TOKEN",KTYPE_STACK,request->get(request,"TOKEN"),KTYPE_STACK);
        response->put(response,"ERROR",KTYPE_STACK,"OFF",KTYPE_STACK);
        onProtocolError((char *) token, jinterface, request, response);
        destructor(jinterface);
        kmap_free(request);
        kmap_free(response);
    }
    if(token_ != NULL){
        (*env)->ReleaseStringUTFChars(env, token_, token);
    }
    if(id_ != NULL){
        (*env)->ReleaseStringUTFChars(env, id_, id);
    }
    if(key_ != NULL){
        (*env)->ReleaseStringUTFChars(env, key_, key);
    }
    if(option_ != NULL){
        (*env)->ReleaseStringUTFChars(env, option_, option);
    }
    if(value_ != NULL){
        (*env)->ReleaseStringUTFChars(env, value_, value);
    }
    if(operator_ != NULL){
        (*env)->ReleaseStringUTFChars(env, operator_, operator);
    }
}

JNIEXPORT void JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_protocol_1connection_1id(JNIEnv *env, jobject instance,
                                                                                         jobject protocol_interface,
                                                                                         jstring token_,
                                                                                         jstring key_,
                                                                                         jstring type_) {
    if(interface == NULL){
        return;
    }
    const char *token = token_ == NULL ? NULL : (*env)->GetStringUTFChars(env, token_, 0);
    const char *key = key_ == NULL ? NULL : (*env)->GetStringUTFChars(env, key_, 0);
    const char *type = type_ == NULL ? NULL : (*env)->GetStringUTFChars(env, type_, 0);
    Jobject *jinterface = kmemory_alloc(sizeof(Jobject));
    jinterface->native_handler_global = (*env)->NewGlobalRef(env,protocol_interface);
    if(!interface->protocol_connection_id(interface, jinterface, (char *) token, (char *) key, (char *) type)){
        Kmap *request = interface->protocol_request(interface,PROTOCOL_CONNECTION_ID,token,key,type);
        Kmap *response = kson_parse("{}");
        response->put(response,"DATA_TYPE",KTYPE_STACK,request->get(request,"DATA_TYPE"),KTYPE_STACK);
        response->put(response,"TOKEN",KTYPE_STACK,request->get(request,"TOKEN"),KTYPE_STACK);
        response->put(response,"ERROR",KTYPE_STACK,"OFF",KTYPE_STACK);
        onProtocolError((char *) token, jinterface, request, response);
        destructor(jinterface);
        kmap_free(request);
        kmap_free(response);
    }
    if(token_ != NULL){
        (*env)->ReleaseStringUTFChars(env, token_, token);
    }
    if(key_ != NULL){
        (*env)->ReleaseStringUTFChars(env, key_, key);
    }
    if(type_ != NULL){
        (*env)->ReleaseStringUTFChars(env, type_, type);
    }
}

JNIEXPORT void JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_protocol_1message_1set(JNIEnv *env, jobject instance,
                                                                                       jobject protocol_interface,
                                                                                       jstring token_,
                                                                                       jstring conection_id_,
                                                                                       jstring message_type_,
                                                                                       jstring forward_id_,
                                                                                       jstring reply_date_,
                                                                                       jstring data_1_,
                                                                                       jstring data_2_) {
    if(interface == NULL){
        return;
    }
    const char *token = token_ == NULL ? NULL : (*env)->GetStringUTFChars(env, token_, 0);
    const char *conection_id = conection_id_ == NULL ? NULL : (*env)->GetStringUTFChars(env, conection_id_, 0);
    const char *message_type = message_type_ == NULL ? NULL : (*env)->GetStringUTFChars(env, message_type_, 0);
    const char *forward_id = forward_id_ == NULL ? NULL : (*env)->GetStringUTFChars(env, forward_id_, 0);
    const char *reply_date = reply_date_ == NULL ? NULL : (*env)->GetStringUTFChars(env, reply_date_, 0);
    const char *data_1 = data_1_ == NULL ? NULL : (*env)->GetStringUTFChars(env, data_1_, 0);
    const char *data_2 = data_2_ == NULL ? NULL : (*env)->GetStringUTFChars(env, data_2_, 0);
    Jobject *jinterface = kmemory_alloc(sizeof(Jobject));
    jinterface->native_handler_global = (*env)->NewGlobalRef(env,protocol_interface);
    if(!interface->protocol_message_set(interface, jinterface, (char *) token, (char *) conection_id, (char *) message_type, (char *) forward_id, (char *) reply_date, (char *) data_1, (char *) data_2)){
        Kmap *request = interface->protocol_request(interface,PROTOCOL_MESSAGE_SET,token,conection_id,message_type,forward_id,reply_date,data_1,data_2);
        Kmap *response = kson_parse("{}");
        response->put(response,"DATA_TYPE",KTYPE_STACK,request->get(request,"DATA_TYPE"),KTYPE_STACK);
        response->put(response,"TOKEN",KTYPE_STACK,request->get(request,"TOKEN"),KTYPE_STACK);
        response->put(response,"ERROR",KTYPE_STACK,"OFF",KTYPE_STACK);
        onProtocolError((char *) token, jinterface, request, response);
        destructor(jinterface);
        kmap_free(request);
        kmap_free(response);
    }
    if(token_ != NULL){
        (*env)->ReleaseStringUTFChars(env, token_, token);
    }
    if(conection_id_ != NULL){
        (*env)->ReleaseStringUTFChars(env, conection_id_, conection_id);
    }
    if(message_type_ != NULL){
        (*env)->ReleaseStringUTFChars(env, message_type_, message_type);
    }
    if(forward_id_ != NULL){
        (*env)->ReleaseStringUTFChars(env, forward_id_, forward_id);
    }
    if(reply_date_ != NULL){
        (*env)->ReleaseStringUTFChars(env, reply_date_, reply_date);
    }
    if(data_1_ != NULL){
        (*env)->ReleaseStringUTFChars(env, data_1_, data_1);
    }
    if(data_2_ != NULL){
        (*env)->ReleaseStringUTFChars(env, data_2_, data_2);
    }
}

JNIEXPORT void JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_protocol_1message_1handle(JNIEnv *env, jobject instance,
                                                                                          jobject protocol_interface,
                                                                                          jstring token_,
                                                                                          jstring conection_id_,
                                                                                          jstring handle_) {
    if(interface == NULL){
        return;
    }
    const char *token = token_ == NULL ? NULL : (*env)->GetStringUTFChars(env, token_, 0);
    const char *conection_id = conection_id_ == NULL ? NULL : (*env)->GetStringUTFChars(env, conection_id_, 0);
    const char *handle = handle_ == NULL ? NULL : (*env)->GetStringUTFChars(env, handle_, 0);
    Jobject *jinterface = kmemory_alloc(sizeof(Jobject));
    jinterface->native_handler_global = (*env)->NewGlobalRef(env,protocol_interface);
    if(!interface->protocol_message_handle(interface, jinterface, (char *) token, (char *) conection_id, (char *) handle)){
        Kmap *request = interface->protocol_request(interface,PROTOCOL_MESSAGE_HANDLE,token,conection_id,handle);
        Kmap *response = kson_parse("{}");
        response->put(response,"DATA_TYPE",KTYPE_STACK,request->get(request,"DATA_TYPE"),KTYPE_STACK);
        response->put(response,"TOKEN",KTYPE_STACK,request->get(request,"TOKEN"),KTYPE_STACK);
        response->put(response,"ERROR",KTYPE_STACK,"OFF",KTYPE_STACK);
        onProtocolError((char *) token, jinterface, request, response);
        destructor(jinterface);
        kmap_free(request);
        kmap_free(response);
    }
    if(token_ != NULL){
        (*env)->ReleaseStringUTFChars(env, token_, token);
    }
    if(conection_id_ != NULL){
        (*env)->ReleaseStringUTFChars(env, conection_id_, conection_id);
    }
    if(handle_ != NULL){
        (*env)->ReleaseStringUTFChars(env, handle_, handle);
    }
}

JNIEXPORT jint JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_stream_1update(JNIEnv *env, jobject instance,
                                                                               jstring link_,
                                                                               jobject stream_interface) {
    if(interface == NULL){
        return 0;
    }
    const char *link = (*env)->GetStringUTFChars(env, link_, 0);
    Jobject *jinterface = kmemory_alloc(sizeof(Jobject));
    jinterface->native_handler_global = (*env)->NewGlobalRef(env,stream_interface);
    int result = interface->stream_update(interface, jinterface, (char *) link, destructor);
    (*env)->ReleaseStringUTFChars(env, link_, link);
    return result;
}

JNIEXPORT void JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_stream_1set(JNIEnv *env, jobject instance,
                                                                            jobject stream_interface,
                                                                            jstring link_,
                                                                            jstring path_,
                                                                            jint calltime) {
    if(interface == NULL){
        return;
    }
    const char *link = (*env)->GetStringUTFChars(env, link_, 0);
    const char *path = (*env)->GetStringUTFChars(env, path_, 0);
    Jobject *jinterface = kmemory_alloc(sizeof(Jobject));
    jinterface->native_handler_global = (*env)->NewGlobalRef(env,stream_interface);
    if(!interface->stream_set(interface, jinterface, (char *) link, (char *) path, calltime)){
        onStreamError((char *) link, jinterface);
        destructor(jinterface);
    }
    (*env)->ReleaseStringUTFChars(env, link_, link);
    (*env)->ReleaseStringUTFChars(env, path_, path);
}

JNIEXPORT void JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_stream_1get(JNIEnv *env, jobject instance,
                                                                            jobject stream_interface,
                                                                            jstring link_,
                                                                            jstring path_,
                                                                            jint calltime) {
    if(interface == NULL){
        return;
    }
    const char *link = (*env)->GetStringUTFChars(env, link_, 0);
    const char *path = (*env)->GetStringUTFChars(env, path_, 0);
    Jobject *jinterface = kmemory_alloc(sizeof(Jobject));
    jinterface->native_handler_global = (*env)->NewGlobalRef(env,stream_interface);
    if(!interface->stream_get(interface, jinterface, (char *) link, (char *) path, calltime)){
        onStreamError((char *) link, jinterface);
        destructor(jinterface);
    }
    (*env)->ReleaseStringUTFChars(env, link_, link);
    (*env)->ReleaseStringUTFChars(env, path_, path);
}

JNIEXPORT jstring JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_stream_1sets(JNIEnv *env, jobject instance) {
    if(interface == NULL){
        return NULL;
    }
    Kmap *result_map = interface->stream_sets(interface);
    char *result = kson_pack(result_map);
    kmap_free(result_map);
    return (*env)->NewStringUTF(env, result == NULL ? "" : result);
}

JNIEXPORT jstring JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_stream_1gets(JNIEnv *env, jobject instance) {
    if(interface == NULL){
        return NULL;
    }
    Kmap *result_map = interface->stream_gets(interface);
    char *result = kson_pack(result_map);
    kmap_free(result_map);
    return (*env)->NewStringUTF(env, result == NULL ? "" : result);
}

JNIEXPORT jint JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_stream_1cancel(JNIEnv *env, jobject instance,
                                                                               jstring link_) {
    if(interface == NULL){
        return 0;
    }
    const char *link = (*env)->GetStringUTFChars(env, link_, 0);
    int result = interface->stream_cancel(interface, (char *) link);
    (*env)->ReleaseStringUTFChars(env, link_, link);
    return result;
}

JNIEXPORT jstring JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_stream_1state(JNIEnv *env, jobject instance,
                                                                              jstring link_) {
    if(interface == NULL){
        return NULL;
    }
    const char *link = (*env)->GetStringUTFChars(env, link_, 0);
    Kmap *result_map = interface->stream_state(interface,(char *) link);
    char *result = kson_pack(result_map);
    kmap_free(result_map);
    (*env)->ReleaseStringUTFChars(env, link_, link);
    return (*env)->NewStringUTF(env, result == NULL ? "" : result);
}

JNIEXPORT jint JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_stream_1clear(JNIEnv *env, jobject instance) {
    if(interface == NULL){
        return 0;
    }
    return interface->stream_clear(interface);
}

JNIEXPORT jlong JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_stream_1size(JNIEnv *env, jobject instance) {
    if(interface == NULL){
        return -1;
    }
    return interface->stream_size(interface);
}

JNIEXPORT jstring JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_stream_1getdata(JNIEnv *env, jobject instance,
                                                                                jstring link_) {
    if(interface == NULL){
        return NULL;
    }
    const char *link = (*env)->GetStringUTFChars(env, link_, 0);
    char *result = interface->stream_getdata(interface, (char *) link);
    (*env)->ReleaseStringUTFChars(env, link_, link);
    return (*env)->NewStringUTF(env, result == NULL ? "" : result);
}

JNIEXPORT jint JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_stream_1setdata(JNIEnv *env, jobject instance,
                                                                                jstring link_,
                                                                                jstring state_) {
    if(interface == NULL){
        return 0;
    }
    const char *link = (*env)->GetStringUTFChars(env, link_, 0);
    const char *state = (*env)->GetStringUTFChars(env, state_, 0);
    int result = interface->stream_setdata(interface, (char *) link, (char *) state);
    (*env)->ReleaseStringUTFChars(env, link_, link);
    (*env)->ReleaseStringUTFChars(env, state_, state);
    return result;
}

JNIEXPORT jstring JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_stream_1file(JNIEnv *env, jobject instance,
                                                                             jstring link_,
                                                                             jint recreate) {
    if(interface == NULL){
        return NULL;
    }
    const char *link = (*env)->GetStringUTFChars(env, link_, 0);
    char *result = interface->stream_file(interface, (char *) link, recreate);
    (*env)->ReleaseStringUTFChars(env, link_, link);
    return (*env)->NewStringUTF(env, result == NULL ? "" : result);
}

JNIEXPORT jstring JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_stream_1temp(JNIEnv *env, jobject instance,
                                                                             jint recreate) {
    if(interface == NULL){
        return NULL;
    }
    char *result = interface->stream_temp(interface, recreate);
    return (*env)->NewStringUTF(env, result == NULL ? "" : result);
}

JNIEXPORT jstring JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_stream_1encode(JNIEnv *env, jobject instance,
                                                                               jstring name_,
                                                                               jstring size_,
                                                                               jstring mime_,
                                                                               jstring checksum_) {
    if(interface == NULL){
        return NULL;
    }
    const char *name = (*env)->GetStringUTFChars(env, name_, 0);
    const char *size = (*env)->GetStringUTFChars(env, size_, 0);
    const char *mime = (*env)->GetStringUTFChars(env, mime_, 0);
    const char *checksum = (*env)->GetStringUTFChars(env, checksum_, 0);
    char *result = interface->stream_encode(interface, (char *) name, (char *) size, (char *) mime, (char *) checksum);
    (*env)->ReleaseStringUTFChars(env, name_, name);
    (*env)->ReleaseStringUTFChars(env, size_, size);
    (*env)->ReleaseStringUTFChars(env, mime_, mime);
    (*env)->ReleaseStringUTFChars(env, checksum_, checksum);
    return (*env)->NewStringUTF(env, result == NULL ? "" : result);
}

JNIEXPORT jstring JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_stream_1decode(JNIEnv *env, jobject instance,
                                                                               jstring link_) {
    if(interface == NULL){
        return NULL;
    }
    const char *link = (*env)->GetStringUTFChars(env, link_, 0);
    Kmap *result_map = interface->stream_decode(interface, (char *) link);
    char *result = kson_pack(result_map);
    kmap_free(result_map);
    (*env)->ReleaseStringUTFChars(env, link_, link);
    return (*env)->NewStringUTF(env, result);
}

JNIEXPORT jint JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_caster_1bundle(JNIEnv *env, jobject instance,
                                                                               jint index,
                                                                               jobject caster_interface) {
    if(interface == NULL){
        return 0;
    }
    Jobject *jinterface = kmemory_alloc(sizeof(Jobject));
    jinterface->native_handler_global = (*env)->NewGlobalRef(env,caster_interface);
    if(interface->bundles[index] != NULL){
        destructor(interface->bundles[index]);
    }
    interface->bundles[index] = jinterface;
    return 1;
}

JNIEXPORT jint JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_caster_1tune(JNIEnv *env,
                                                                             jobject instance,
                                                                             jstring target_id_,
                                                                             jint tune) {
    if(interface == NULL){
        return 0;
    }
    CasterTune caster_tune;
    switch (tune){
        case 0:
            caster_tune = CASTER_TUNE_OPEN;
            break;
        case 1:
            caster_tune = CASTER_TUNE_RDWR;
            break;
        case 2:
            caster_tune = CASTER_TUNE_RDONLY;
            break;
        case 3:
            caster_tune = CASTER_TUNE_WRONLY;
            break;
        case 4:
            caster_tune = CASTER_TUNE_NONE;
            break;
        case 5:
            caster_tune = CASTER_TUNE_WAIT;
            break;
        case 6:
            caster_tune = CASTER_TUNE_CLOSE;
            break;
        default:
            caster_tune = CASTER_TUNE_CLOSE;
            break;
    }
    const char *target_id = (*env)->GetStringUTFChars(env, target_id_, 0);
    int result = interface->caster_tune(interface, (char *) target_id, caster_tune);
    (*env)->ReleaseStringUTFChars(env, target_id_, target_id);
    return result;
}

JNIEXPORT jint JNICALL
Java_cham_koliber_ir_chamui_ChaMUI_ChaMCore_ChaMNativeInterface_caster_1cast(JNIEnv *env, jobject instance,
                                                                             jbyteArray data_,
                                                                             jint size, jint end) {
    if(interface == NULL){
        return 0;
    }
    jbyte *data = (*env)->GetByteArrayElements(env, data_, NULL);
    int result = interface->caster_cast(interface,data,size,end);
    (*env)->ReleaseByteArrayElements(env, data_, data, 0);
    return result;
}