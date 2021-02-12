cmake_minimum_required(VERSION 3.4.1)

include_directories(${CMAKE_SOURCE_DIR}/src/main/jni/ChaM/include)
include_directories(${CMAKE_SOURCE_DIR}/src/main/jni/ChaM/libs/include)
add_library(ChaM SHARED
    ${CMAKE_SOURCE_DIR}/src/main/jni/ChaM/ChaM/interface.c
    ${CMAKE_SOURCE_DIR}/src/main/jni/ChaM/ChaM/protocol.c
    ${CMAKE_SOURCE_DIR}/src/main/jni/ChaM/ChaM/stream.c
    ${CMAKE_SOURCE_DIR}/src/main/jni/ChaM/ChaM/caster.c
    ${CMAKE_SOURCE_DIR}/src/main/jni/ChaM/ChaM/media.c
    ${CMAKE_SOURCE_DIR}/src/main/jni/ChaM/ChaM/utils/dsa/karray.c
    ${CMAKE_SOURCE_DIR}/src/main/jni/ChaM/ChaM/utils/dsa/klist.c
    ${CMAKE_SOURCE_DIR}/src/main/jni/ChaM/ChaM/utils/dsa/kmap.c
    ${CMAKE_SOURCE_DIR}/src/main/jni/ChaM/ChaM/utils/dsa/kson.c
    ${CMAKE_SOURCE_DIR}/src/main/jni/ChaM/ChaM/utils/ipc/kmsg.c
    ${CMAKE_SOURCE_DIR}/src/main/jni/ChaM/ChaM/utils/ipc/ksem.c
    ${CMAKE_SOURCE_DIR}/src/main/jni/ChaM/ChaM/utils/net/ktcp.c
    ${CMAKE_SOURCE_DIR}/src/main/jni/ChaM/ChaM/utils/net/kudp.c
    ${CMAKE_SOURCE_DIR}/src/main/jni/ChaM/ChaM/utils/tools/kmemory.c
    ${CMAKE_SOURCE_DIR}/src/main/jni/ChaM/ChaM/utils/tools/kprocessor.c
    ${CMAKE_SOURCE_DIR}/src/main/jni/ChaM/libs/sqlite3.c

    ${CMAKE_SOURCE_DIR}/src/main/jni/chamnativeapi.c
)
target_link_libraries(ChaM log)