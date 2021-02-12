#include "cham_util.h"

CassCluster *driver_cluster = NULL;
CassSession *driver_session = NULL;
CassFuture *driver_future = NULL;

void node_toggle(int is_add){
    MYSQL *connection;
    if((connection = mysql_init(NULL)) == NULL){
        return;
    }
    if(mysql_real_connect(connection,AUTH_DBMS_HOST,AUTH_DBMS_USER,AUTH_DBMS_PASS,AUTH_DBMS_DATABASE,0,NULL,0) == NULL){
        return;
    }
    char query[1024];
    if(is_add){
        strcpy(query , "INSERT INTO NODES(HOST , NAME , TYPE) VALUES('");
        strcat(query , NODE_HOST);
        strcat(query , "','");
        strcat(query , "ok','STREAM')");
    }else{
        strcpy(query , "DELETE FROM NODES WHERE HOST='");
        strcat(query , NODE_HOST);
        strcat(query , "' AND NAME='ok' AND TYPE='STREAM'");
    }
    mysql_query(connection,"CREATE TABLE IF NOT EXITS NODES(HOST TEXT , NAME TEXT , TYPE TEXT , PRIMARY KEY(HOST(50) , NAME(50)))");
    mysql_query(connection,query);
    mysql_close(connection);
}

int driver_connect(){
    driver_cluster = cass_cluster_new();
    driver_session = cass_session_new();
    cass_cluster_set_contact_points(driver_cluster,"127.0.0.1");
    cass_cluster_set_protocol_version(driver_cluster,3);
    cass_cluster_set_latency_aware_routing(driver_cluster,cass_true);
    cass_cluster_set_token_aware_routing(driver_cluster,cass_true);
    cass_cluster_set_num_threads_io(driver_cluster,4);
    cass_cluster_set_queue_size_io(driver_cluster,128000);
    cass_cluster_set_max_connections_per_host(driver_cluster,5);
    cass_cluster_set_pending_requests_high_water_mark(driver_cluster,128000);
    cass_cluster_set_tcp_nodelay(driver_cluster,cass_true);
    cass_cluster_set_tcp_keepalive(driver_cluster,cass_true,1800);
    cass_cluster_set_reconnect_wait_time(driver_cluster , 0);
    driver_future = cass_session_connect_keyspace(driver_session,driver_cluster,"cham");
    CassError driver_error = cass_future_error_code(driver_future);
    if(driver_error != CASS_OK){
        printf("Server Cassandra Driver init Terminated !\n Error : %s",cass_error_desc(driver_error));
        return 0;
    }else{
        printf("Server Cassandra Driver init was Successful !\n");
        return 1;
    }
}

void driver_disconnect(){
    if(driver_future != NULL){
        cass_future_free(driver_future);
    }
    if(driver_session != NULL){
        cass_session_close(driver_session);
        cass_session_free(driver_session);
    }
    if(driver_cluster != NULL){
        cass_cluster_free(driver_cluster);
    }
}

int is_client(char *ID , char *SCODE){
    if(ID == NULL || SCODE == NULL){
        return 0;
    }
    CassStatement *query_statement = cass_statement_new("SELECT SCODE FROM USERS WHERE ID=?",1);
    cass_statement_bind_string(query_statement,0,ID);
    CassFuture *query_future = cass_session_execute(driver_session,query_statement);
    if(query_future == NULL || cass_future_error_code(query_future) != CASS_OK){
        return 0;
    }
    const CassResult *query_result = cass_future_get_result(query_future);
    if(query_result == NULL){
        cass_future_free(query_future);
        cass_statement_free(query_statement);
        return 0;
    }
    const char* query_data;
    size_t query_data_length;
    if(cass_result_row_count(query_result) <= 0){
        return 0;
    }
    CassError query_error = cass_value_get_string(
            cass_row_get_column_by_name(cass_result_first_row(query_result),"SCODE"),
            &query_data,
            &query_data_length
    );
    cass_result_free(query_result);
    cass_future_free(query_future);
    cass_statement_free(query_statement);
    if(query_error != CASS_OK){
        return 0;
    }
    if(strcmp(SCODE,query_data) == 0){
        return 1;
    }else{
        return 0;
    }
}

char *hex_to_string(char *hex){
    int hex_length = (int) strlen(hex);
    char chars[3];
    chars[3] = '\0';
    char data[hex_length/2];
    for(int cursor = 0 ; cursor < hex_length ; cursor+=2){
        chars[0] = hex[cursor];
        chars[1] = hex[cursor+1];
        char c = (char) strtol(chars, NULL, 16);
        data[cursor/2] = c;
    }
    memset(hex , 0 , sizeof(char)*hex_length);
    hex = realloc(hex , sizeof(char)*hex_length/2);
    memcpy(hex, &data, (size_t) (hex_length / 2));
    return hex;
}

char *string_to_hex(char *string){
    char data[strlen(string)*2+1];
    memset(data,0,strlen(string)*2+1);
    char chars[3];
    for(int cursor = 0 ; cursor < strlen(string) ; cursor++){
        sprintf(chars, "%02X", string[cursor]);
        strcat(data,chars);
    }
    string = realloc(string , strlen(string)*2+1);
    memset(string,0,strlen(string)*2+1);
    strcpy(string,data);
    return string;
}

Kmap *link_parse(char *link_text){
    size_t link_length = strlen(link_text) / 2;
    link_text = hex_to_string(link_text);
    RC4_KEY crypto_key;
    RC4_set_key(&crypto_key, (int) strlen(PUBLIC_CRYPTO_KEY), (const unsigned char *) PUBLIC_CRYPTO_KEY);
    char *clear_text = malloc((link_length+1)*sizeof(char));
    memset(clear_text , 0 , (link_length+1));
    RC4(&crypto_key,link_length, (const unsigned char *) link_text, (unsigned char *) clear_text);
    Kmap *kmap = kson_parse(clear_text);
    if(kmap_indexof(kmap , "MIME") != -1 && kmap_indexof(kmap , "SIZE") != -1 && kmap_indexof(kmap , "CHECKSUM") != -1 && kmap_indexof(kmap , "ID") != -1){
        return kmap;
    }else{
        kmap_free(kmap);
        return NULL;
    }
}

char *link_make(Kmap *kmap , int is_set){
    char *link_path = malloc(sizeof(char) * 4096);
    strcpy(link_path,BASE_PATH);
    strcat(link_path , string_to_hex(kmap_get(kmap , "MIME")));
    strcat(link_path,"/");
    strcat(link_path , kmap_get(kmap , "SIZE"));
    strcat(link_path,"/");
    strcat(link_path , kmap_get(kmap , "CHECKSUM"));
    strcat(link_path,"/");
    char *COMMAND = malloc(sizeof(char)*4096);
    strcpy(COMMAND,"mkdir -p ");
    strcat(COMMAND,link_path);
    system(COMMAND);
    free(COMMAND);
    DIR *dir;
    struct dirent *ent;
    if((dir = opendir(link_path)) != NULL){
        while((ent = readdir(dir)) != NULL){
            if(strcmp(ent->d_name , kmap_get(kmap , "ID")) == 0 || is_set == 0){
                strcat(link_path , kmap_get(kmap , "ID"));
                return link_path;
            }
        }
        strcat(link_path , kmap_get(kmap , "ID"));
        FILE *file = fopen(link_path, "ab+");
        fclose(file);
        return link_path;
    }
    return NULL;
}