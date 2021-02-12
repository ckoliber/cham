#ifndef CHAMSOCKET_H
#define CHAMSOCKET_H

#include <QObject>
#include "chamdatabase.h"
#include "chamsecurity.h"
#include "chamsignalhandler.h"
#include <QTcpSocket>
#include <QUdpSocket>
#include <QThread>
#include <pthread.h>

class ChaMSocket : public QObject
{
    Q_OBJECT
public:
    explicit ChaMSocket();
    void startSocket();
    bool handShake();
    QString readData();
    void startCommunication();
    bool getSessionState();
    void callReply(QString FromID,QString FromIP,QString FromPort,QString SecurityCode,QString CallType);
    void callRequest(QString CallType,QString TargetID);
    void writeData(QString data);
    void startCall(bool meStartCall,QString CallType,QString TargetID,quint16 TargetPort,QHostAddress TargetHost);
    Q_INVOKABLE void sendMessage(QString DataType,QString MDate,QString TargetType,QString TargetID,QString TextData,QString EmojiCode,QString A);
    Q_INVOKABLE void searchTarget(QString TargetType,QString TargetID);
    Q_INVOKABLE void loadTarget(QString TargetType,QString TargetID);


signals:
    void message(QString DataType,
                 QString MDate,
                 QString From,
                 QString FromGroupOrChannelMemberID,
                 QString FromType,
                 QString TextData,
                 QString EmojiCode,
                 QString ContactName,
                 QString ContactPhone,
                 QString LocationAlt,
                 QString LocationLat,
                 QString LocationLong,
                 QString StreamLink,
                 QString BufferSource,
                 QString StreamType);
    void searchResult(QString found);
    void nodeInfo(QString NodeType,QString NodeID,QString Key,QString Value);

public slots:
    void initSocket();


private:
    QThread connectorThread;
    QThread readerThread;
    ChaMDatabase *database;
    ChaMSecurity *security;
    ChaMSignalHandler *signalHandler;
    QTcpSocket *server_socket;
    QUdpSocket *call_socket;
    QString serverHost = "127.0.0.1";
    quint16 serverPort = 1418;
    quint16 rendezvousPort = 1419;
    bool session_is_on = false;
};

#endif // CHAMSOCKET_H
