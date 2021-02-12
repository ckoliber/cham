#include <QGuiApplication>
#include <QQmlApplicationEngine>
#include <QObject>
#include <QQmlContext>
#include <QQuickWindow>
#include "mainreceiver.h"
#include <QDir>
#include "chamdatabase.h"
#include <QDebug>
#include "chamsignalhandler.h"
#include "chamsocket.h"
#include <QQuickStyle>
#include <chamdate.h>
#include <QDateTime>
#include <QCamera>
#include <qtwebengineglobal.h>

int main(int argc, char *argv[]){
    ChaMDate *chdate = new ChaMDate;
    ChaMDatabase *database = new ChaMDatabase;
    database->initDirs();
    database->initClient("JAFAR","0","0","9380851109","I'm good :)","ali","98","dsf","1","");
    ChaMSocket *socket = new ChaMSocket;
    socket->startSocket();
    QCoreApplication::setAttribute(Qt::AA_EnableHighDpiScaling);
    QGuiApplication app(argc, argv);
    QtWebEngine::initialize();
    QQmlApplicationEngine engine;
    QQuickStyle::setStyle("Material");
    QPM_INIT(engine);
    ChaMSignalHandler *signalHandler = new ChaMSignalHandler;
    if(!database->getClient("BOOLSETUP").contains("T")){
        engine.rootContext()->setContextProperty("SIGNALHANDLER",signalHandler);
        engine.rootContext()->setContextProperty("SOCKETHANDLER",socket);
        engine.rootContext()->setContextProperty("DATEHANDLER",chdate);
        engine.load(QUrl(QLatin1String("qrc:/Chat/ChatView.qml")));
    }else{
        engine.load(QUrl(QLatin1String("qrc:/main.qml")));
        QObject *object = engine.rootObjects().value(0);
        QQuickWindow *window = qobject_cast<QQuickWindow *>(object);
        MainReceiver *signalHandler = new MainReceiver(&engine);
        QObject::connect(window,SIGNAL(getCountries()),signalHandler,SLOT(slotGetCountries()));
        QObject::connect(window,SIGNAL(sendCode(QString,QString,QString)),signalHandler,SLOT(slotSendCode(QString,QString,QString)));
    }
    return app.exec();
}
