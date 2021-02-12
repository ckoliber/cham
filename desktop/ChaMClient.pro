QT += qml quick multimedia quickcontrols2 webengine xml svg
QTPLUGIN += qsvg

CONFIG += c++11
SOURCES += main.cpp \
    mainreceiver.cpp \
    chamsocket.cpp \
    chamsignalhandler.cpp \
    chamsecurity.cpp \
    chamhttp.cpp \
    chamdatabase.cpp \
    chamdate.cpp

RESOURCES += qml.qrc
include(deployment.pri)
DEFINES += QPM_INIT\\(E\\)=\"E.addImportPath(QStringLiteral(\\\"qrc:/\\\"));\"
include(material/material.pri)


HEADERS += \
    mainreceiver.h \
    chamsocket.h \
    chamsignalhandler.h \
    chamsecurity.h \
    chamdatabase.h \
    chamdate.h \
    chamhttp.h


