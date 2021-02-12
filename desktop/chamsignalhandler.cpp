#include "chamsignalhandler.h"

ChaMSignalHandler::ChaMSignalHandler(){
    database = new ChaMDatabase;
    security = new ChaMSecurity;
}

void ChaMSignalHandler::loadMainList(){
    int LengthCLT = database->getNodeLength("CLT");
    int LengthGPT = database->getNodeLength("GPT");
    int LengthCHT = database->getNodeLength("CHT");
    if(LengthCLT > 0){
        for(int a = 0 ; a < LengthCLT; a++){
            int LengthCLTMSGS = database->getNodeMessagesLength("CLT",a);
            if(LengthCLTMSGS > 0){
                try {
                    QString jsonstring = database->getNodeMessage("CLT",a,LengthCLTMSGS-1);
                    QJsonDocument doc = QJsonDocument::fromJson(jsonstring.toUtf8());
                    QJsonObject dataJson = doc.object();
                    emit message(
                                  dataJson.value("DT").toString(),
                                  dataJson.value("DATE").toString(),
                                  dataJson.value("FROM").toString(),
                                  dataJson.value("FRID").toString(),
                                  dataJson.value("FRT").toString(),
                                  dataJson.value("TXT").toString(),
                                  dataJson.value("EMJ").toString(),
                                  dataJson.value("CNA").toString(),
                                  dataJson.value("CPH").toString(),
                                  dataJson.value("ALT").toString(),
                                  dataJson.value("LAT").toString(),
                                  dataJson.value("LONG").toString(),
                                  dataJson.value("LNK").toString(),
                                  dataJson.value("SRC").toString(),
                                  dataJson.value("ST").toString(),
                                  dataJson.value("SELF").toString().contains("T"),
                                  LengthCLTMSGS
                                 );


                } catch (QException e) {}
            }
        }
    }

    if(LengthGPT > 0){
        for(int a = 0 ; a < LengthGPT; a++){
            int LengthGPTMSGS = database->getNodeMessagesLength("GPT",a);
            if(LengthGPTMSGS > 0){
                try {
                    QString jsonstring = database->getNodeMessage("GPT",a,LengthGPTMSGS-1);
                    QJsonDocument doc = QJsonDocument::fromJson(jsonstring.toUtf8());
                    QJsonObject dataJson = doc.object();

                    emit message(
                                  dataJson.value("DT").toString(),
                                  dataJson.value("DATE").toString(),
                                  dataJson.value("FROM").toString(),
                                  dataJson.value("FRID").toString(),
                                  dataJson.value("FRT").toString(),
                                  dataJson.value("TXT").toString(),
                                  dataJson.value("EMJ").toString(),
                                  dataJson.value("CNA").toString(),
                                  dataJson.value("CPH").toString(),
                                  dataJson.value("ALT").toString(),
                                  dataJson.value("LAT").toString(),
                                  dataJson.value("LONG").toString(),
                                  dataJson.value("LNK").toString(),
                                  dataJson.value("SRC").toString(),
                                  dataJson.value("ST").toString(),
                                  dataJson.value("SELF").toString().contains("T"),
                                  LengthGPTMSGS
                                 );


                } catch (QException e) {}
            }
        }
    }

    if(LengthCHT > 0){
        for(int a = 0 ; a < LengthCHT; a++){
            int LengthCHTMSGS = database->getNodeMessagesLength("CHT",a);
            if(LengthCHTMSGS > 0){
                try {
                    QString jsonstring = database->getNodeMessage("CHT",a,LengthCHTMSGS-1);
                    QJsonDocument doc = QJsonDocument::fromJson(jsonstring.toUtf8());
                    QJsonObject dataJson = doc.object();

                    emit message(
                                  dataJson.value("DT").toString(),
                                  dataJson.value("DATE").toString(),
                                  dataJson.value("FROM").toString(),
                                  dataJson.value("FRID").toString(),
                                  dataJson.value("FRT").toString(),
                                  dataJson.value("TXT").toString(),
                                  dataJson.value("EMJ").toString(),
                                  dataJson.value("CNA").toString(),
                                  dataJson.value("CPH").toString(),
                                  dataJson.value("ALT").toString(),
                                  dataJson.value("LAT").toString(),
                                  dataJson.value("LONG").toString(),
                                  dataJson.value("LNK").toString(),
                                  dataJson.value("SRC").toString(),
                                  dataJson.value("ST").toString(),
                                  dataJson.value("SELF").toString().contains("T"),
                                  LengthCHTMSGS
                                 );


                } catch (QException e) {}
            }
        }
    }

}

void ChaMSignalHandler::loadItemDetails(QString TargetID,QString TargetType){
    emit setTargetPage(TargetID,TargetType);
    emit clearDetail();
    QString TargetFolder =  security->CHHash(TargetID);
    int messagesLength = database->getNodeMessagesLengthByNodeName(TargetType,TargetFolder);
    for(int a = 0 ; a < messagesLength ; a++ ){
        try {
            QString jsonstring = database->getNodeMessageByNodeName(TargetType,TargetFolder,a);
            QJsonDocument doc = QJsonDocument::fromJson(jsonstring.toUtf8());
            QJsonObject dataJson = doc.object();
            emit messageDetail(dataJson.value("DT").toString(),
                               dataJson.value("DATE").toString(),
                               dataJson.value("FROM").toString(),
                               dataJson.value("FRID").toString(),
                               dataJson.value("FRT").toString(),
                               dataJson.value("TXT").toString(),
                               dataJson.value("EMJ").toString(),
                               dataJson.value("CNA").toString(),
                               dataJson.value("CPH").toString(),
                               dataJson.value("ALT").toString(),
                               dataJson.value("LAT").toString(),
                               dataJson.value("LONG").toString(),
                               dataJson.value("LNK").toString(),
                               dataJson.value("SRC").toString(),
                               dataJson.value("ST").toString(),
                               dataJson.value("SELF").toString().contains("T"));
        } catch (QException e) {}
    }
}

void ChaMSignalHandler::removeFile(QString path,QString type){
    if(type == "AUD"){
        QString pathh = QStandardPaths::writableLocation(QStandardPaths::TempLocation);
        QFile file(pathh+"/ChaMAudio."+getFileFormatString(path));
        file.remove();
    }else if(type == "IMG"){
        QFile file(path.replace("file://",""));
        file.remove();
    }else if(type == "VDO"){
        QFile file(path.replace("file:///","/"));
        file.remove();
    }else if(type == "BUF"){
        QFile file(path.replace("file://",""));
        file.remove();
    }
}

QString ChaMSignalHandler::saveFile(QString path, QString type){
    if(type == "AUD"){
        // not use path


    }else if(type == "IMG"){


    }else if(type == "VDO"){


    }else if(type == "BUF"){


    }
}

QString ChaMSignalHandler::copyFile(QString path, QString type){
    QString pathh = QStandardPaths::writableLocation(QStandardPaths::TempLocation);
    if(type == "AUD"){
        QFile file(pathh+"/ChaMAudio."+getFileFormatString(path));
        file.remove();
        bool result = QFile::copy(path.replace("file://",""),pathh+"/ChaMAudio."+getFileFormatString(path));
        if(result){
            return pathh+"/ChaMAudio."+getFileFormatString(path);
        }else{
            return "";
        }
    }else if(type == "IMG"){
        QFile file(pathh+"/ChaMImage."+getFileFormatString(path));
        file.remove();
        bool result = QFile::copy(path.replace("file://",""),pathh+"/ChaMImage."+getFileFormatString(path));
        if(result){
            return pathh+"/ChaMImage."+getFileFormatString(path);
        }else{
            return "";
        }
    }else if(type == "VDO"){
        QFile file(pathh+"/ChaMVideo."+getFileFormatString(path));
        file.remove();
        bool result = QFile::copy(path.replace("file://",""),pathh+"/ChaMVideo."+getFileFormatString(path));
        if(result){
            return pathh+"/ChaMVideo."+getFileFormatString(path);
        }else{
            return "";
        }
    }else if(type == "BUF"){
        QFile file(pathh+"/ChaMBuffer."+getFileFormatString(path));
        file.remove();
        bool result = QFile::copy(path.replace("file://",""),pathh+"/ChaMBuffer."+getFileFormatString(path));
        if(result){
            return pathh+"/ChaMBuffer."+getFileFormatString(path);
        }else{
            return "";
        }
    }
}

void ChaMSignalHandler::resumeRecordAudio(){
    audioRecorder->record();
}

void ChaMSignalHandler::pauseRecordAudio(){
    audioRecorder->pause();
}

void ChaMSignalHandler::startRecordAudio(){
    audioRecorder = new QAudioRecorder;
    QAudioEncoderSettings audioSettings;
    audioSettings.setCodec("audio/mpeg");
    audioSettings.setQuality(QMultimedia::HighQuality);
    audioRecorder->setEncodingSettings(audioSettings);
    QString path = QStandardPaths::writableLocation(QStandardPaths::MusicLocation);
    QFile file(path+"/ChaMAudio.mp3");
    file.remove();
    file.open(QIODevice::ReadWrite);
    file.close();
    audioRecorder->setOutputLocation(QUrl::fromLocalFile(path+"/ChaMAudio.mp3"));

}

QString ChaMSignalHandler::stopRecordAudio(){
    audioRecorder->stop();
    QString path = QStandardPaths::writableLocation(QStandardPaths::MusicLocation);
    return path+"/ChaMAudio.mp3";
}

void ChaMSignalHandler::processWave(QString path){
    QAudioFormat desiredFormat;
    desiredFormat.setChannelCount(2);
    desiredFormat.setCodec("audio/x-raw");
    desiredFormat.setSampleType(QAudioFormat::UnSignedInt);
    desiredFormat.setSampleRate(48000);
    desiredFormat.setSampleSize(16);
    audioDecoder = new QAudioDecoder;
    audioDecoder->setAudioFormat(desiredFormat);
    audioDecoder->setSourceFilename(path);
    connect(audioDecoder,SIGNAL(bufferReady()),this,SLOT(audioDecoderReadBuffer()));
    connect(audioDecoder,SIGNAL(finished()),this,SLOT(audioDecoderEnd()));
    audioDecoder->start();
}

void ChaMSignalHandler::audioDecoderReadBuffer(){
    QAudioBuffer buffer = audioDecoder->read();
    QAudioBuffer::S16S *data = buffer.data<QAudioBuffer::S16S>();
    for(int a = 0 ; a < buffer.frameCount() ; a += 50){
        emit waveBuffer(data[a].average()/2);
    }
}

void ChaMSignalHandler::audioDecoderEnd(){
    emit waveEnd();
}

QString ChaMSignalHandler::cropImage(QString path, int x, int y, int width, int height){
    QString tempPath = QStandardPaths::writableLocation(QStandardPaths::TempLocation);
    QImage image(path.replace("file://",""));
    QImage copy;
    copy = image.copy(x,y,width,height);
    QFile file(tempPath+"/ChaMImage.png");
    file.remove();
    copy.save(tempPath+"/ChaMImage.png","PNG",-1);
    QFile resultFile(path.replace("file://",""));
    resultFile.remove();
    return tempPath+"/ChaMImage.png";
}

QString ChaMSignalHandler::getFileFormatString(QString path){
    QString format = "";
    for(int a = path.length() - 1 ; a >= 0; a--){
        if(path.at(a) != '.'){
            format += path.at(a);
        }else{
            break;
        }
    }
    QString form = "";
    for(int a = format.length() - 1 ; a >= 0 ; a--){
        form += format.at(a);
    }
    return form;
}

int ChaMSignalHandler::getFileFormat(QString path){
    QString form = getFileFormatString(path);
    if(
            form == "mp3" ||
            form == "aac" ||
            form == "ogg" ||
            form == "aa" ||
            form == "aax" ||
            form == "amr" ||
            form == "m4a" ||
            form == "wav" ||
            form == "aiff" ||
            form == "wv" ||
            form == "flac" ||
            form == "tta" ||
            form == "spx"){
        return 0;
    }else if(
             form == "png" ||
             form == "jpg" ||
             form == "jpeg"
             ){
        return 1;
    }else if(
             form == "3gp" ||
             form == "mp4" ||
             form == "mpg" ||
             form == "mpeg" ||
             form == "mpe" ||
             form == "mpv" ||
             form == "mv2" ||
             form == "m4p" ||
             form == "m4v"
             ){
        return 2;
    }else{
        return 3;
    }
}

qint64 ChaMSignalHandler::getFileSize(QString path){
    QFile file(path.replace("file://",""));
    return file.size();
}

QString ChaMSignalHandler::getFileName(QString path){
    QStringList parts = path.split("/");
    return parts.at(parts.size()-1);
}

QString ChaMSignalHandler::recentEmojies(QString emojiUnicode){
    QString recentEmoji = database->getClient("REMOJI");
    QStringList list = recentEmoji.split("|");
    if(emojiUnicode != ""){
        QString res = emojiUnicode+"|";
        for(int a = 0 ;a < list.size() ; a++){
            if(list[a] != emojiUnicode){
                if(list[a] != ""){
                    res += list[a]+"|";
                }
            }
        }
        database->setClient("REMOJI",res);
        return res;
    }else{
        return recentEmoji;
    }
}

QString ChaMSignalHandler::cutMedia(QString path, quint64 startMil, quint64 endMil,int mediaType){
    if(mediaType == 0){
        // audio
        char* chpath = path.replace("file://","").toLatin1().data();


    }else{
        // video

    }

}

QStringList ChaMSignalHandler::loadUser(QString TargetType, QString TargetID){
    QString name = database->getNode(TargetType,TargetID,"NAME");
    QString bio = database->getNode(TargetType,TargetID,"BIO");
    QString pic = database->getNode(TargetType,TargetID,"PIC");
    QStringList list;
    if(name != NULL){
        if(TargetType == "CLT"){
            QString phone = database->getNode(TargetType,TargetID,"PHONE");
            list.insert(0,name);
            list.insert(1,bio);
            list.insert(2,phone);
            return list;
        }else{
            QString members = database->getNode(TargetType,TargetID,"MEMBERS");
            QString admins = database->getNode(TargetType,TargetID,"ADMINS");
            QString boss = database->getNode(TargetType,TargetID,"ADMINS");
            list.insert(0,name);
            list.insert(1,bio);
            list.insert(2,admins);
            list.insert(3,members);
            list.insert(4,boss);
            return list;
        }
    }else{
        return list;
    }
}
