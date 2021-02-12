#ifndef CHAMSERVER_CHAM_UTIL_H
#define CHAMSERVER_CHAM_UTIL_H

#define PUBLIC_CRYPTO_KEY "f6ae7cb1ed049cb6e7f262b28f49a257402a99084016ada0cc995da07c7f4def49943e7d8e727af27665aa7c76c962787cfc34a1cdf696101241e73f6f1e5f48"
//#define BASE_PATH      "/root/Stream/"
#define BASE_PATH      "/home/koliber/Stream/"

#define AUTH_DBMS_HOST      "127.0.0.1"
#define AUTH_DBMS_USER      "root"
#define AUTH_DBMS_PASS      "140277mhn"
#define AUTH_DBMS_DATABASE  "cham"

#define NODE_HOST           "127.0.0.1"

#include "dirent.h"
#include "unistd.h"
#include <stdio.h>
#include <stdlib.h>
#include "string.h"
#include "openssl/rc4.h"
#include "klb/dsa/kson.h"
#include "cassandra.h"
#include "mysql.h"

void node_toggle(int is_add);

int driver_connect();
void driver_disconnect();
int is_client(char *ID , char *SCODE);

char *hex_to_string(char *hex);
char *string_to_hex(char *string);

Kmap *link_parse(char *link_text);
char *link_make(Kmap *kmap , int is_set);

#endif //CHAMSERVER_CHAM_UTIL_H
