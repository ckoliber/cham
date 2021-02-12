#include "mainreceiver.h"
#include <QDebug>
#include <QQmlApplicationEngine>
#include <QFile>
#include <QList>
#include <QByteArray>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QLoggingCategory>
#include <QtNetwork>
#include "chamhttp.h"
#include <QJsonDocument>
#include <QJsonObject>
QQmlApplicationEngine *engine;
MainReceiver::MainReceiver(QQmlApplicationEngine *engineinp)
{    
    engine = engineinp;
}

void MainReceiver::slotGetCountries(){
    QObject *object = engine->rootObjects().value(0);
    QString fileAddr = "/root/QtProjects/ChaM/countries/CountriesCodes.txt";
    QFile file(fileAddr);
    file.open(QIODevice::ReadOnly);
    QList<QByteArray> datas;
    QVariant name,code,length;
    for(int i = 0 ; i < 232 ; i++){
        datas = file.readLine().split(';');
        name = QString(datas[2]);
        code = QString(datas[0]);
        if(datas.length() >= 4){
            length = QString(datas[3]);
        }else{
            length = "-1";
        }
        QMetaObject::invokeMethod(object, "setCountry",Q_ARG(QVariant, name),Q_ARG(QVariant, code),Q_ARG(QVariant, length));
    }
    ChaMHttp *http = new ChaMHttp;
    connect(http,SIGNAL(onReply(QString)),this,SLOT(slotCurrentCountry(QString)));
    http->GetRequest(QUrl("http://ip-api.com/json"));
}

void MainReceiver::slotCurrentCountry(QString reply){
    QJsonDocument jsonResponse = QJsonDocument::fromJson(reply.toUtf8());
    QJsonObject jsonObject = jsonResponse.object();
    QVariant countryCode = jsonObject.value("countryCode");
    QString fileAddr = "/root/QtProjects/ChaM/countries/CountriesCodes.txt";
    QFile file(fileAddr);
    file.open(QIODevice::ReadOnly);
    QList<QByteArray> datas;
    QVariant code;
    for(int i = 0 ; i < 232 ; i++){
        datas = file.readLine().split(';');
        code = QString(datas[1]);
        if(code == countryCode){
            QObject *object = engine->rootObjects().value(0);
            QMetaObject::invokeMethod(object, "setCurrentCountry",Q_ARG(QVariant, QVariant(i)));
            break;
        }
    }
}

void MainReceiver::slotSendCode(QString code,QString phone,QString ccode){
    ChaMHttp *http = new ChaMHttp;
    QByteArray byteArray;
    byteArray.append("CD="+code+"&PN="+phone+"&CC="+ccode);
    connect(http,SIGNAL(onReply(QString)),this,SLOT(slotSendCodeReply(QString)));
    http->PostRequest(QUrl("http://10.0.3.2:1418/register"),byteArray);
}

void MainReceiver::slotSendCodeReply(QString reply){
    QJsonDocument jsonResponse = QJsonDocument::fromJson(reply.toUtf8());
    QJsonObject jsonObject = jsonResponse.object();
    QVariant res = jsonObject.value("RES");
    if(res == "OK"){
        QVariant id = jsonObject.value("ID");
        QVariant scode = jsonObject.value("SCODE");
        // setup data

    }else if(res == "NO"){
        QObject *object = engine->rootObjects().value(0);
        QMetaObject::invokeMethod(object, "sendCodeError",Q_ARG(QVariant, QVariant("INC")));
    }else{
        QObject *object = engine->rootObjects().value(0);
        QMetaObject::invokeMethod(object, "sendCodeError",Q_ARG(QVariant, QVariant("NET")));
    }
}
