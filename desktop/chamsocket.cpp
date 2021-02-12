#include "chamsocket.h"
#include <QTcpSocket>
#include <QDebug>
#include <QByteArray>
#include <QDataStream>
#include <QThread>
#include <QException>
#include <QJsonObject>
#include <QJsonDocument>

ChaMSocket::ChaMSocket(){
    database = new ChaMDatabase;
    server_socket = new QTcpSocket;
    call_socket = new QUdpSocket;
    security = new ChaMSecurity;
    signalHandler = new ChaMSignalHandler;
}

void ChaMSocket::startSocket(){
    connect(&connectorThread, SIGNAL(started()), this, SLOT(initSocket()),Qt::DirectConnection);
    connectorThread.start();
}

bool ChaMSocket::handShake(){
    try {
        QJsonObject *handshakeObj = new QJsonObject;
        handshakeObj->insert("ID",database->getClient("ID"));
        handshakeObj->insert("SC",database->getClient("SCODE"));
        handshakeObj->insert("DT","INT");
        QJsonDocument doc2(*handshakeObj);
        server_socket->write(QByteArray(QString(doc2.toJson(QJsonDocument::Compact)).toUtf8()));
        QString result = readData();
        if(result.contains("LGI") && result.contains("OK")){
            session_is_on = true;
            return true;
        }else{
            session_is_on = false;
            return false;
        }
    } catch (QException ignored) {
        session_is_on = false;
        return false;
    }
}

QString ChaMSocket::readData(){
    server_socket->waitForReadyRead();
    return QString(server_socket->readAll());
}

void ChaMSocket::initSocket(){
    if(!getSessionState()){
        session_is_on = false;
        server_socket->close();
        server_socket->connectToHost(serverHost,serverPort);
        if (!server_socket->waitForConnected(3000)) {
            QThread::sleep(2);
            initSocket();
        }else{
            if(handShake()){
                startCommunication();
                QThread::sleep(2);
                initSocket();
            }else{
                QThread::sleep(2);
                initSocket();
            }
        }
    }else{
        QThread::sleep(2);
        initSocket();
    }
}

bool ChaMSocket::getSessionState(){
    return (session_is_on && server_socket->isOpen());
}

void ChaMSocket::startCommunication(){
    while(getSessionState()){
        QString data = readData();
        qDebug() << data;
        try{
            QJsonDocument jsonResponse = QJsonDocument::fromJson(data.toUtf8());
            QJsonObject jsonObject = jsonResponse.object();
            QString DT = jsonObject.value("DT").toString();
            if (DT.contains("ERR")){
                qDebug() << "Error : " <<  jsonObject.value("D").toString();
            }else if(DT.contains("TXT")){
                database->insertNewMessage(DT,
                                           jsonObject.value("DATE").toString(),
                                           jsonObject.value("FROM").toString(),
                                           jsonObject.value("FRID").toString(),
                                           jsonObject.value("FRT").toString(),
                                           jsonObject.value("TXT").toString(),
                                           NULL,
                                           NULL,
                                           NULL,
                                           NULL,
                                           NULL,
                                           NULL,
                                           NULL,
                                           NULL,
                                           NULL,
                                           false);
                emit message(DT,
                                       jsonObject.value("DATE").toString(),
                                       jsonObject.value("FROM").toString(),
                                       jsonObject.value("FRID").toString(),
                                       jsonObject.value("FRT").toString(),
                                       jsonObject.value("TXT").toString(),
                                       NULL,
                                       NULL,
                                       NULL,
                                       NULL,
                                       NULL,
                                       NULL,
                                       NULL,
                                       NULL,
                                       NULL);


            }else if(DT.contains("EMJ")){
                database->insertNewMessage(DT,
                                           jsonObject.value("DATE").toString(),
                                           jsonObject.value("FROM").toString(),
                                           jsonObject.value("FRID").toString(),
                                           jsonObject.value("FRT").toString(),
                                           NULL,
                                           jsonObject.value("EMJ").toString(),
                                           NULL,
                                           NULL,
                                           NULL,
                                           NULL,
                                           NULL,
                                           NULL,
                                           NULL,
                                           NULL,
                                           false);
                emit message(DT,
                                       jsonObject.value("DATE").toString(),
                                       jsonObject.value("FROM").toString(),
                                       jsonObject.value("FRID").toString(),
                                       jsonObject.value("FRT").toString(),
                                       NULL,
                                       jsonObject.value("EMJ").toString(),
                                       NULL,
                                       NULL,
                                       NULL,
                                       NULL,
                                       NULL,
                                       NULL,
                                       NULL,
                                       NULL);
            }else if(DT.contains("LOC")){

                database->insertNewMessage(DT,
                                           jsonObject.value("DATE").toString(),
                                           jsonObject.value("FROM").toString(),
                                           jsonObject.value("FRID").toString(),
                                           jsonObject.value("FRT").toString(),
                                           NULL,
                                           NULL,
                                           NULL,
                                           NULL,
                                           jsonObject.value("ALT").toString(),
                                           jsonObject.value("LAT").toString(),
                                           jsonObject.value("LONG").toString(),
                                           NULL,
                                           NULL,
                                           NULL,
                                           false);
                emit message(DT,
                                       jsonObject.value("DATE").toString(),
                                       jsonObject.value("FROM").toString(),
                                       jsonObject.value("FRID").toString(),
                                       jsonObject.value("FRT").toString(),
                                       NULL,
                                       NULL,
                                       NULL,
                                       NULL,
                                       jsonObject.value("ALT").toString(),
                                       jsonObject.value("LAT").toString(),
                                       jsonObject.value("LONG").toString(),
                                       NULL,
                                       NULL,
                                       NULL);

            }else if(DT.contains("CNT")){


                database->insertNewMessage(DT,
                                           jsonObject.value("DATE").toString(),
                                           jsonObject.value("FROM").toString(),
                                           jsonObject.value("FRID").toString(),
                                           jsonObject.value("FRT").toString(),
                                           NULL,
                                           NULL,
                                           jsonObject.value("CNA").toString(),
                                           jsonObject.value("CPH").toString(),
                                           NULL,
                                           NULL,
                                           NULL,
                                           NULL,
                                           NULL,
                                           NULL,
                                           false);
                emit message(DT,
                                       jsonObject.value("DATE").toString(),
                                       jsonObject.value("FROM").toString(),
                                       jsonObject.value("FRID").toString(),
                                       jsonObject.value("FRT").toString(),
                                       NULL,
                                       NULL,
                                       jsonObject.value("CNA").toString(),
                                       jsonObject.value("CPH").toString(),
                                       NULL,
                                       NULL,
                                       NULL,
                                       NULL,
                                       NULL,
                                       NULL);
            }else if(DT.contains("DST")){
                database->insertNewMessage(DT,
                                           jsonObject.value("DATE").toString(),
                                           jsonObject.value("FROM").toString(),
                                           jsonObject.value("FRID").toString(),
                                           jsonObject.value("FRT").toString(),
                                           NULL,
                                           NULL,
                                           NULL,
                                           NULL,
                                           NULL,
                                           NULL,
                                           NULL,
                                           jsonObject.value("LNK").toString(),
                                           "",
                                           jsonObject.value("ST").toString(),
                                           false);
                emit message(DT,
                                       jsonObject.value("DATE").toString(),
                                       jsonObject.value("FROM").toString(),
                                       jsonObject.value("FRID").toString(),
                                       jsonObject.value("FRT").toString(),
                                       NULL,
                                       NULL,
                                       NULL,
                                       NULL,
                                       NULL,
                                       NULL,
                                       NULL,
                                       jsonObject.value("LNK").toString(),
                                       "",
                                       jsonObject.value("ST").toString());


            }else if(DT.contains("VCM")){
                callReply(jsonObject.value("FID").toString(),jsonObject.value("IP").toString(),jsonObject.value("PORT").toString(),jsonObject.value("SEC").toString(),"V");
            }else if(DT.contains("ACM")){
                callReply(jsonObject.value("FID").toString(),jsonObject.value("IP").toString(),jsonObject.value("PORT").toString(),jsonObject.value("SEC").toString(),"A");
            }else if(DT.contains("PRO")){
                // Result of Profile Load
                if(jsonObject.value("TOT").toString().contains("CLT")){
                // get link of picture
                    database->insertNode("CLT",jsonObject.value("ID").toString(),"NAME",jsonObject.value("NAME").toString());
                    database->insertNode("CLT",jsonObject.value("ID").toString(),"BIO",jsonObject.value("BIO").toString());
                    database->insertNode("CLT",jsonObject.value("ID").toString(),"ID",jsonObject.value("ID").toString());
                    database->insertNode("CLT",jsonObject.value("ID").toString(),"PIC",jsonObject.value("PIC").toString());
                    emit nodeInfo("CLT",jsonObject.value("ID").toString(),"NAME",jsonObject.value("NAME").toString());
                    emit nodeInfo("CLT",jsonObject.value("ID").toString(),"BIO",jsonObject.value("BIO").toString());
                    emit nodeInfo("CLT",jsonObject.value("ID").toString(),"ID",jsonObject.value("ID").toString());
                    emit nodeInfo("CLT",jsonObject.value("ID").toString(),"PIC",jsonObject.value("PIC").toString());

                }else if(jsonObject.value("TOT").toString().contains("GPT")){
                    database->insertNode("GPT",jsonObject.value("ID").toString(),"NAME",jsonObject.value("NAME").toString());
                    database->insertNode("GPT",jsonObject.value("ID").toString(),"BIO",jsonObject.value("BIO").toString());
                    database->insertNode("GPT",jsonObject.value("ID").toString(),"ID",jsonObject.value("ID").toString());
                    database->insertNode("GPT",jsonObject.value("ID").toString(),"ADMINS",jsonObject.value("ADMINS").toString());
                    database->insertNode("GPT",jsonObject.value("ID").toString(),"BOSS",jsonObject.value("BOSS").toString());
                    database->insertNode("GPT",jsonObject.value("ID").toString(),"MEMBERS",jsonObject.value("MEMBERS").toString());
                    database->insertNode("GPT",jsonObject.value("ID").toString(),"PIC",jsonObject.value("PIC").toString());
                    emit nodeInfo("GPT",jsonObject.value("ID").toString(),"NAME",jsonObject.value("NAME").toString());
                    emit nodeInfo("GPT",jsonObject.value("ID").toString(),"BIO",jsonObject.value("BIO").toString());
                    emit nodeInfo("GPT",jsonObject.value("ID").toString(),"ID",jsonObject.value("ID").toString());
                    emit nodeInfo("GPT",jsonObject.value("ID").toString(),"ADMINS",jsonObject.value("ADMINS").toString());
                    emit nodeInfo("GPT",jsonObject.value("ID").toString(),"BOSS",jsonObject.value("BOSS").toString());
                    emit nodeInfo("GPT",jsonObject.value("ID").toString(),"MEMBERS",jsonObject.value("MEMBERS").toString());
                    emit nodeInfo("GPT",jsonObject.value("ID").toString(),"PIC",jsonObject.value("PIC").toString());
                }else if(jsonObject.value("TOT").toString().contains("CHT")){
                    database->insertNode("CHT",jsonObject.value("ID").toString(),"NAME",jsonObject.value("NAME").toString());
                    database->insertNode("CHT",jsonObject.value("ID").toString(),"BIO",jsonObject.value("BIO").toString());
                    database->insertNode("CHT",jsonObject.value("ID").toString(),"ID",jsonObject.value("ID").toString());
                    database->insertNode("CHT",jsonObject.value("ID").toString(),"ADMINS",jsonObject.value("ADMINS").toString());
                    database->insertNode("CHT",jsonObject.value("ID").toString(),"BOSS",jsonObject.value("BOSS").toString());
                    database->insertNode("CHT",jsonObject.value("ID").toString(),"MEMBERS",jsonObject.value("MEMBERS").toString());
                    database->insertNode("CHT",jsonObject.value("ID").toString(),"PIC",jsonObject.value("PIC").toString());
                    emit nodeInfo("CHT",jsonObject.value("ID").toString(),"NAME",jsonObject.value("NAME").toString());
                    emit nodeInfo("CHT",jsonObject.value("ID").toString(),"BIO",jsonObject.value("BIO").toString());
                    emit nodeInfo("CHT",jsonObject.value("ID").toString(),"ID",jsonObject.value("ID").toString());
                    emit nodeInfo("CHT",jsonObject.value("ID").toString(),"ADMINS",jsonObject.value("ADMINS").toString());
                    emit nodeInfo("CHT",jsonObject.value("ID").toString(),"BOSS",jsonObject.value("BOSS").toString());
                    emit nodeInfo("CHT",jsonObject.value("ID").toString(),"MEMBERS",jsonObject.value("MEMBERS").toString());
                    emit nodeInfo("CHT",jsonObject.value("ID").toString(),"PIC",jsonObject.value("PIC").toString());
                }


            }else if(DT.contains("SCH")){
                emit searchResult(jsonObject.value("D").toString());
            }
        }catch(QException e){}
    }
    session_is_on = false;
}

void ChaMSocket::callReply(QString FromID,QString FromIP,QString FromPort,QString SecurityCode,QString CallType){
    // Call Type -> V - A
    try{
        call_socket->connectToHost(QHostAddress(FromIP),(qint16)FromPort.toInt());
        call_socket->waitForDisconnected(1000);
        QJsonObject *peerHandshakeObject = new QJsonObject;
        peerHandshakeObject->insert("ID",database->getClient("ID"));
        peerHandshakeObject->insert("SEC",SecurityCode);
        peerHandshakeObject->insert("DT",CallType+"CM");
        QJsonDocument peerHandshakeDocument(*peerHandshakeObject);
        call_socket->writeDatagram(QByteArray(QString(peerHandshakeDocument.toJson(QJsonDocument::Compact)).toUtf8()),QHostAddress(FromIP),FromPort.toInt());
        call_socket->waitForBytesWritten(1000);
        QByteArray peerHandshakeReply;
        call_socket->waitForReadyRead(1000);
        peerHandshakeReply.resize(call_socket->pendingDatagramSize());
        call_socket->readDatagram(peerHandshakeReply.data(),peerHandshakeReply.size());
        QJsonDocument peerHandshakeReplyDocument = QJsonDocument::fromJson(QString(peerHandshakeReply).toUtf8());
        QJsonObject peerHandshakeReplyObject = peerHandshakeReplyDocument.object();
        if(peerHandshakeReplyObject.value("ID").toString() == FromID && peerHandshakeReplyObject.value("DT").toString() == CallType+"CM" && peerHandshakeReplyObject.value("SEC").toString() == SecurityCode){
            startCall(false,CallType,FromID,FromPort.toInt(),QHostAddress(FromIP));
        }else{
            // cant handshake !!
            call_socket->close();
        }
    }catch(QException e){}
}

void ChaMSocket::callRequest(QString CallType,QString TargetID){
    try {
        QJsonObject *serverHandshakeObject = new QJsonObject;
        serverHandshakeObject->insert("ID",database->getClient("ID"));
        serverHandshakeObject->insert("SC",database->getClient("SCODE"));
        serverHandshakeObject->insert("DT",CallType+"CM");
        serverHandshakeObject->insert("TO", TargetID);
        QJsonDocument serverHandshakeDocument(*serverHandshakeObject);
        call_socket->writeDatagram(QByteArray(QString(serverHandshakeDocument.toJson(QJsonDocument::Compact)).toUtf8()),QHostAddress(serverHost),rendezvousPort);
        call_socket->waitForBytesWritten(1000);
        QByteArray serverHandshakeReply;
        call_socket->waitForReadyRead(1000);
        serverHandshakeReply.resize(call_socket->pendingDatagramSize());
        call_socket->readDatagram(serverHandshakeReply.data(), serverHandshakeReply.size());
        QJsonDocument serverHandshakeReplyDocument = QJsonDocument::fromJson(QString(serverHandshakeReply).toUtf8());
        QJsonObject serverHandshakeReplyObject = serverHandshakeReplyDocument.object();
        if(serverHandshakeReplyObject.value("RES").toString() == "OK"){
            QString secCode = serverHandshakeReplyObject.value("SEC").toString();
            QByteArray peerHandshake;
            call_socket->waitForReadyRead(5000);
            peerHandshake.resize(call_socket->pendingDatagramSize());
            QHostAddress peerIP;
            quint16 peerPort;
            call_socket->readDatagram(peerHandshake.data(), peerHandshake.size(),&peerIP, &peerPort);
            QJsonDocument peerHandshakeDocument = QJsonDocument::fromJson(QString(peerHandshake).toUtf8());
            QJsonObject peerHandshakeObject = peerHandshakeDocument.object();
            if(peerHandshakeObject.value("ID").toString() == TargetID && peerHandshakeObject.value("DT").toString() == CallType+"CM" && peerHandshakeObject.value("SEC").toString() == secCode){
                QJsonObject *endHandshakeObject = new QJsonObject;
                endHandshakeObject->insert("ID",database->getClient("ID"));
                endHandshakeObject->insert("SEC",secCode);
                endHandshakeObject->insert("DT",CallType+"CM");
                QJsonDocument endHandshakeDocument(*endHandshakeObject);
                call_socket->writeDatagram(QByteArray(QString(endHandshakeDocument.toJson(QJsonDocument::Compact)).toUtf8()),peerIP,peerPort);
                call_socket->waitForBytesWritten(1000);
                startCall(true,CallType,TargetID,peerPort,peerIP);
            }else{
                call_socket->close();
                // send cant handshake !!
            }
        }else{
            // send cant call !!
            call_socket->close();
        }
    }catch (QException e) {}

}

void ChaMSocket::startCall(bool meStartCall,QString CallType,QString TargetID,quint16 TargetPort,QHostAddress TargetHost){
    // CallType == V - A
    emit signalHandler->call(meStartCall,CallType,TargetID,TargetHost,TargetPort);

}

void ChaMSocket::writeData(QString data){
    server_socket->write(QByteArray(data.toUtf8()));
    server_socket->waitForBytesWritten();
}

void ChaMSocket::sendMessage(QString DataType,QString MDate,QString TargetType,QString TargetID,QString TextData,QString EmojiCode,QString A){

    database->insertNewMessage(DataType,
                               MDate,
                               TargetID,
                               TargetID,
                               TargetType,
                               TextData,
                               EmojiCode,
                               NULL,//ContactName
                               NULL,//ContactPhone
                               NULL,//Alt
                               NULL,//LAT
                               NULL,//Long
                               NULL,//StreamLink
                               NULL,//BufferSource
                               NULL,//StreamType
                               "T"//isMe
                               );

    QJsonObject *messageObject = new QJsonObject;

//    switch (DataType) {
//    case "TXT":
//        messageObject->insert("DT","TXT");
//        messageObject->insert("TO",TargetID);
//        messageObject->insert("TOT",TargetType);
//        messageObject->insert("TXT",TextData);
//        break;
//    case "EMJ":
//        messageObject->insert("DT","EMJ");
//        messageObject->insert("TO",TargetID);
//        messageObject->insert("TOT",TargetType);
//        messageObject->insert("EMJ",EmojiCode);
//        break;
//    case "CNT":
//        messageObject->insert("DT","CNT");
//        messageObject->insert("TO",TargetID);
//        messageObject->insert("TOT",TargetType);
//        messageObject->insert("CNA",ContactName);
//        messageObject->insert("CPH",ContactPhone);
//        break;
//    case "LOC":
//        messageObject->insert("DT","LOC");
//        messageObject->insert("TO",TargetID);
//        messageObject->insert("TOT",TargetType);
//        messageObject->insert("ALT",LocationAlt);
//        messageObject->insert("LAT",LocationLat);
//        messageObject->insert("LONG",LocationLong);

//        break;
//    case "DST":


//        break;
//    }

    QJsonDocument doc2(*messageObject);
    writeData(QString(doc2.toJson(QJsonDocument::Compact)));

}

void ChaMSocket::searchTarget(QString TargetType, QString TargetID){
    QJsonObject *messageObject = new QJsonObject;
    messageObject->insert("DT","SCH");
    messageObject->insert("TO",TargetID);
    messageObject->insert("TOT",TargetType);
    QJsonDocument messageDocument(*messageObject);
    writeData(QString(messageDocument.toJson(QJsonDocument::Compact)));
}

void ChaMSocket::loadTarget(QString TargetType, QString TargetID){
    QJsonObject *messageObject = new QJsonObject;
    messageObject->insert("DT","PRO");
    messageObject->insert("PR",TargetID);
    messageObject->insert("PRT",TargetType);
    QJsonDocument messageDocument(*messageObject);
    writeData(QString(messageDocument.toJson(QJsonDocument::Compact)));
}

