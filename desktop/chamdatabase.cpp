#include "chamdatabase.h"
#include <QDir>
#include <QFile>
#include <QException>
#include <QTextStream>
#include <QString>
#include <QDebug>
#include <QVector>
#include <QStringList>
#include <QFileInfo>
#include <QJsonDocument>
#include <QJsonObject>
#include "chamsecurity.h"
QString MAINDIR = "/home/koliber/.ChaM";
QString MAINVDIR = "/home/koliber/ChaM";
QString ROOTDIR = "/home/koliber";
ChaMSecurity *security;
ChaMDatabase::ChaMDatabase(QObject *parent) : QObject(parent)
{
    security = new ChaMSecurity;
}

void ChaMDatabase::initDirs(){
    try {
        QDir rootDir(ROOTDIR);
        rootDir.mkdir("ChaM");
        rootDir.mkdir("ChaM/Images");
        rootDir.mkdir("ChaM/Videos");
        rootDir.mkdir("ChaM/Audios");
        rootDir.mkdir("ChaM/Files");
        rootDir.mkdir(".ChaM");
        rootDir.mkdir(".ChaM/Images");
        rootDir.mkdir(".ChaM/Videos");
        rootDir.mkdir(".ChaM/Audios");
        rootDir.mkdir(".ChaM/Files");
        rootDir.mkdir(".ChaM/Data");
        rootDir.mkdir(".ChaM/Data/Setting");
        rootDir.mkdir(".ChaM/Data/Client");
        rootDir.mkdir(".ChaM/Data/Message");
        rootDir.mkdir(".ChaM/Data/Message/CLT");
        rootDir.mkdir(".ChaM/Data/Message/GPT");
        rootDir.mkdir(".ChaM/Data/Message/CHT");
        rootDir.mkdir(".ChaM/Data/Profile");
        rootDir.mkdir(".ChaM/Data/Profile/CLT");
        rootDir.mkdir(".ChaM/Data/Profile/GPT");
        rootDir.mkdir(".ChaM/Data/Profile/CHT");
    }catch (QException e){}
}

void ChaMDatabase::initClient(QString Name,QString LastSeen,QString Picture,QString Phone,QString Bio,QString ID, QString CCode,QString SCode,QString BoolSetup,QString Blocks){
    try {
        QFile PHONE(MAINDIR+"/Data/Client/PHONE");
        QFile BIO(MAINDIR+"/Data/Client/BIO");
        QFile CCODE(MAINDIR+"/Data/Client/CCODE");
        QFile SCODE(MAINDIR+"/Data/Client/SCODE");
        QFile BOOLSETUP(MAINDIR+"/Data/Client/BOOLSETUP");
        QFile IDF(MAINDIR+"/Data/Client/ID");
        QFile BLOCKS(MAINDIR+"/Data/Client/BLOCKS");
        QFile NAME(MAINDIR+"/Data/Client/NAME");
        QFile LASTSEEN(MAINDIR+"/Data/Client/LASTSEEN");
        QFile PIC(MAINDIR+"/Data/Client/PIC");

        if(!PHONE.exists()){
            PHONE.open(QIODevice::WriteOnly);
            QTextStream writer(&PHONE);
            writer << Phone;
        }

        if(!BIO.exists()){
            BIO.open(QIODevice::WriteOnly);
            QTextStream writer(&BIO);
            writer << Bio;
        }
        if(!CCODE.exists()){
            CCODE.open(QIODevice::WriteOnly);
            QTextStream writer(&CCODE);
            writer << CCode;
        }
        if(!SCODE.exists()){
            SCODE.open(QIODevice::WriteOnly);
            QTextStream writer(&SCODE);
            writer << SCode;
        }
        if(!BOOLSETUP.exists()){
            BOOLSETUP.open(QIODevice::WriteOnly);
            QTextStream writer(&BOOLSETUP);
            writer << BoolSetup;
        }
        if(!IDF.exists()){
            IDF.open(QIODevice::WriteOnly);
            QTextStream writer(&IDF);
            writer << ID;
        }
        if(!BLOCKS.exists()){
            BLOCKS.open(QIODevice::WriteOnly);
            QTextStream writer(&BLOCKS);
            writer << Blocks;
        }
        if(!NAME.exists()){
            NAME.open(QIODevice::WriteOnly);
            QTextStream writer(&NAME);
            writer << Name;
        }
        if(!LASTSEEN.exists()){
            LASTSEEN.open(QIODevice::WriteOnly);
            QTextStream writer(&LASTSEEN);
            writer << LastSeen;
        }
        if(!PIC.exists()){
            PIC.open(QIODevice::WriteOnly);
            QTextStream writer(&PIC);
            writer << Picture;
        }
    } catch (QException e) {}
}

void ChaMDatabase::initSetting(QString Theme,QString Language,QString AutoUpdate, QString MDAutoDownloadVideo,QString MDAutoDownloadImage,QString MDAutoDownloadAudio,QString MWAutoDownloadVideo,QString MWAutoDownloadImage,QString MWAutoDownloadAudio,QString ChatBackground,QString ListType,QString MessagesNotify,QString MessagesNotifyTone,QString MessagesNotifyVibrate,QString MessagesNotifyLEDColor,QString GPCHNotify,QString GPCHNotifyTone,QString GPCHNotifyVibrate,QString GPCHNotifyLEDColor,QString CallNotify,QString CallNotifyRingtone,QString CallNotifyVibrate,QString DataCrypto){

    try{
        // Keys -> THEME LANGUAGE AUTOUPDATE CHATBACK MDAUTOVDL MDAUTOADL MDAUTOIDL MWAUTOVDL MWAUTOADL MWAUTOIDL DATACRYPTO
        // Notify -> MSGNOTIFI GPCHNOTIFI CALLNOTIFI
        // Notify Tone -> MSGNOTIFITONE GPCHNOTIFITONE CALLNOTIFITONE
        // Notify Vibrate -> MSGNOTIFIVIB GPCHNOTIFIVIB CALLNOTIFIVIB
        // Notify LED -> MSGNOTIFILED GPCHNOTIFILED

     QFile THEME(MAINDIR+"/Data/Client/THEME");
     QFile LANGUAGE(MAINDIR+"/Data/Client/LANGUAGE");
     QFile AUTOUPDATE(MAINDIR+"/Data/Client/AUTOUPDATE");
     QFile MDAUTOVDL(MAINDIR+"/Data/Client/MDAUTOVDL");
     QFile MDAUTOIDL(MAINDIR+"/Data/Client/MDAUTOIDL");
     QFile MDAUTOADL(MAINDIR+"/Data/Client/MDAUTOADL");
     QFile MWAUTOVDL(MAINDIR+"/Data/Client/MWAUTOVDL");
     QFile MWAUTOIDL(MAINDIR+"/Data/Client/MWAUTOIDL");
     QFile MWAUTOADL(MAINDIR+"/Data/Client/MWAUTOADL");
     QFile CHATBACK(MAINDIR+"/Data/Client/CHATBACK");
     QFile LISTTYPE(MAINDIR+"/Data/Client/LISTTYPE");
     QFile MSGNOTIFI(MAINDIR+"/Data/Client/MSGNOTIFI");
     QFile GPCHNOTIFI(MAINDIR+"/Data/Client/GPCHNOTIFI");
     QFile CALLNOTIFI(MAINDIR+"/Data/Client/CALLNOTIFI");
     QFile MSGNOTIFITONE(MAINDIR+"/Data/Client/MSGNOTIFITONE");
     QFile GPCHNOTIFITONE(MAINDIR+"/Data/Client/GPCHNOTIFITONE");
     QFile CALLNOTIFITONE(MAINDIR+"/Data/Client/CALLNOTIFITONE");
     QFile MSGNOTIFIVIB(MAINDIR+"/Data/Client/MSGNOTIFIVIB");
     QFile GPCHNOTIFIVIB(MAINDIR+"/Data/Client/GPCHNOTIFIVIB");
     QFile CALLNOTIFIVIB(MAINDIR+"/Data/Client/CALLNOTIFIVIB");
     QFile MSGNOTIFILED(MAINDIR+"/Data/Client/MSGNOTIFILED");
     QFile GPCHNOTIFILED(MAINDIR+"/Data/Client/GPCHNOTIFILED");
     QFile DATACRYPTO(MAINDIR+"/Data/Client/DATACRYPTO");
        if(!THEME.exists()){
            THEME.open(QIODevice::WriteOnly);
            QTextStream writer(&THEME);
            writer << Theme;
        }
        if(!LANGUAGE.exists()){
            LANGUAGE.open(QIODevice::WriteOnly);
            QTextStream writer(&LANGUAGE);
            writer << Language;
        }
        if(!AUTOUPDATE.exists()){
            AUTOUPDATE.open(QIODevice::WriteOnly);
            QTextStream writer(&AUTOUPDATE);
            writer << AutoUpdate;
        }
        if(!MDAUTOVDL.exists()){
            MDAUTOVDL.open(QIODevice::WriteOnly);
            QTextStream writer(&MDAUTOVDL);
            writer << MDAutoDownloadVideo;
        }
        if(!MDAUTOIDL.exists()){
            MDAUTOIDL.open(QIODevice::WriteOnly);
            QTextStream writer(&MDAUTOIDL);
            writer << MDAutoDownloadImage;
        }
        if(!MDAUTOADL.exists()){
            MDAUTOADL.open(QIODevice::WriteOnly);
            QTextStream writer(&MDAUTOADL);
            writer << MDAutoDownloadAudio;
        }
        if(!MWAUTOVDL.exists()){
            MWAUTOVDL.open(QIODevice::WriteOnly);
            QTextStream writer(&MWAUTOVDL);
            writer << MWAutoDownloadVideo;
        }
        if(!MWAUTOIDL.exists()){
            MWAUTOIDL.open(QIODevice::WriteOnly);
            QTextStream writer(&MWAUTOIDL);
            writer << MWAutoDownloadImage;
        }
        if(!MWAUTOADL.exists()){
            MWAUTOADL.open(QIODevice::WriteOnly);
            QTextStream writer(&MWAUTOADL);
            writer << MWAutoDownloadAudio;
        }
        if(!CHATBACK.exists()){
            CHATBACK.open(QIODevice::WriteOnly);
            QTextStream writer(&CHATBACK);
            writer << ChatBackground;
        }
        if(!LISTTYPE.exists()){
            LISTTYPE.open(QIODevice::WriteOnly);
            QTextStream writer(&LISTTYPE);
            writer << ListType;
        }
        if(!MSGNOTIFI.exists()){
            MSGNOTIFI.open(QIODevice::WriteOnly);
            QTextStream writer(&MSGNOTIFI);
            writer << MessagesNotify;
        }
        if(!GPCHNOTIFI.exists()){
            GPCHNOTIFI.open(QIODevice::WriteOnly);
            QTextStream writer(&GPCHNOTIFI);
            writer << GPCHNotify;
        }
        if(!CALLNOTIFI.exists()){
            CALLNOTIFI.open(QIODevice::WriteOnly);
            QTextStream writer(&CALLNOTIFI);
            writer << CallNotify;
        }
        if(!MSGNOTIFITONE.exists()){
            MSGNOTIFITONE.open(QIODevice::WriteOnly);
            QTextStream writer(&MSGNOTIFITONE);
            writer << MessagesNotifyTone;
        }
        if(!GPCHNOTIFITONE.exists()){
            GPCHNOTIFITONE.open(QIODevice::WriteOnly);
            QTextStream writer(&GPCHNOTIFITONE);
            writer << GPCHNotifyTone;
        }
        if(!CALLNOTIFITONE.exists()){
            CALLNOTIFITONE.open(QIODevice::WriteOnly);
            QTextStream writer(&CALLNOTIFITONE);
            writer << CallNotifyRingtone;
        }
        if(!MSGNOTIFIVIB.exists()){
            MSGNOTIFIVIB.open(QIODevice::WriteOnly);
            QTextStream writer(&MSGNOTIFIVIB);
            writer << MessagesNotifyVibrate;
        }
        if(!GPCHNOTIFIVIB.exists()){
            GPCHNOTIFIVIB.open(QIODevice::WriteOnly);
            QTextStream writer(&GPCHNOTIFIVIB);
            writer << GPCHNotifyVibrate;
        }
        if(!CALLNOTIFIVIB.exists()){
            CALLNOTIFIVIB.open(QIODevice::WriteOnly);
            QTextStream writer(&CALLNOTIFIVIB);
            writer << CallNotifyVibrate;
        }
        if(!MSGNOTIFILED.exists()){
            MSGNOTIFILED.open(QIODevice::WriteOnly);
            QTextStream writer(&MSGNOTIFILED);
            writer << MessagesNotifyLEDColor;
        }
        if(!GPCHNOTIFILED.exists()){
            GPCHNOTIFILED.open(QIODevice::WriteOnly);
            QTextStream writer(&GPCHNOTIFILED);
            writer << GPCHNotifyLEDColor;
        }
        if(!DATACRYPTO.exists()){
            DATACRYPTO.open(QIODevice::WriteOnly);
            QTextStream writer(&DATACRYPTO);
            writer << DataCrypto;
        }
    } catch (QException e) {}

}

QString ChaMDatabase::getClient(QString Item){
    // Keys -> NAME LASTSEEN PIC PHONE BIO CCODE SCODE BOOLSETUP ID BLOCKS REMOJI
    try {
        QFile itemFile(MAINDIR+"/Data/Client/"+Item);
        if(itemFile.exists()){
            if(!Item.contains("PIC")){
                itemFile.open(QIODevice::ReadWrite);
                QTextStream reader(&itemFile);
                return reader.readAll();
            }else{
                return MAINDIR+"/Data/Client/"+Item;
            }
        }else{
            return NULL;
        }
    } catch (QException e) {
        return NULL;
    }
}

QString ChaMDatabase::getSetting(QString Item){
    try {
        QFile itemFile(MAINDIR+"/Data/Setting/"+Item);
        itemFile.open(QIODevice::ReadOnly);
        QTextStream reader(&itemFile);
        return reader.readAll();
    } catch (QException e) {
        return NULL;
    }
}

void ChaMDatabase::setClient(QString Item,QString Value){
    try {
        QFile clientFile(MAINDIR+"/Data/Client/"+Item);
        clientFile.open(QIODevice::ReadWrite);
        QTextStream writer(&clientFile);
        writer << Value;
    }catch (QException e){}

}

void ChaMDatabase::setSetting(QString Item,QString Value){
    try {
        QFile settingFile(MAINDIR+"/Data/Setting/"+Item);
        settingFile.open(QIODevice::WriteOnly);
        QTextStream writer(&settingFile);
        writer << Value;
    }catch (QException e){}
}

void ChaMDatabase::removeClient(QString Key,QString ItemPos){
    if(Key.contains("BLOCKS") && ItemPos.toInt() >= 0){
        QString blocks = ChaMDatabase::getClient("BLOCKS");
        QStringList blocks_array = blocks.split("\n");
        QVector<QString> vector;
        for(int i = 0 ; i < blocks_array.length() ; i++){
            vector.insert(i,blocks_array.at(i));
        }
        vector.remove(ItemPos.toInt());
        QString res = "";
        for(int j = 0 ; j < vector.size() ; j++){
            res += "\n" + vector.at(j);
        }
        ChaMDatabase::setClient("BLOCKS",res);
    }
}

int ChaMDatabase::getNodeLength(QString NodeType){

    // nodes ->  CLT  GPT  CHT

    try {
        QDir dir(MAINDIR+"/Data/Message/"+NodeType);
        return dir.entryList(QDir::Dirs).size() - 2;
    }catch (QException e){
        return 0;
    }
}

int ChaMDatabase::getNodeMessagesLength(QString NodeType,int Node){
    try {
        QDir dir(MAINDIR+"/Data/Message/"+NodeType);
        QStringList list = dir.entryList(QDir::Dirs);
        QDir dir2(MAINDIR+"/Data/Message/"+NodeType+"/"+list.at(Node+2));
        return dir2.entryList(QDir::Files).size();
    }catch (QException e){
        return 0;
    }
}

QString ChaMDatabase::getNodeMessage(QString NodeType,int Node,int Message){
    try {
        QDir dir(MAINDIR+"/Data/Message/"+NodeType);
        QStringList list = dir.entryList(QDir::Dirs);
        QDir dir2(MAINDIR+"/Data/Message/"+NodeType+"/"+list.at(Node+2));
        QStringList fileslist = dir2.entryList(QDir::Files);
        QFile fileread(MAINDIR+"/Data/Message/"+NodeType+"/"+list.at(Node+2)+"/"+fileslist.at(Message));
        fileread.open(QIODevice::ReadOnly);
        QTextStream reader(&fileread);
        return reader.readAll();
    }catch (QException e){
        return NULL;
    }
}

QString ChaMDatabase::getNodeMessageByNodeName(QString NodeType,QString Node,int Message){
    try {
        QDir dir(MAINDIR+"/Data/Message/"+NodeType+"/"+Node);
        QStringList list = dir.entryList(QDir::Files);
        QFile fileread(MAINDIR+"/Data/Message/"+NodeType+"/"+Node+"/"+list.at(Message));
        fileread.open(QIODevice::ReadOnly);
        QTextStream reader(&fileread);
        return reader.readAll();
    }catch (QException e){
        return NULL;
    }
}

int ChaMDatabase::getNodeMessagesLengthByNodeName(QString NodeType,QString Node){
    try {
        QDir dir(MAINDIR+"/Data/Message/"+NodeType+"/"+Node);
        return dir.entryList(QDir::Files).size();
    }catch (QException e){
        return 0;
    }
}

void ChaMDatabase::insertNewMessage(QString DataType,
                                    QString Date,
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
                                    QString StreamType,
                                    QString isSelf){
    try {
        QString hashfrom = security->CHHash(From);
        QDir folderfrom(MAINDIR+"/Data/Message/"+FromType);
        folderfrom.mkdir(hashfrom);
        QFile datafile(MAINDIR+"/Data/Message/"+FromType+"/"+hashfrom+"/"+Date);
        datafile.open(QIODevice::WriteOnly);
        QJsonObject *jsonObj = new QJsonObject;
        jsonObj->insert("DT",DataType);
        jsonObj->insert("TXT",TextData);
        jsonObj->insert("EMJ",EmojiCode);
        jsonObj->insert("ALT",LocationAlt);
        jsonObj->insert("LAT",LocationLat);
        jsonObj->insert("LONG",LocationLong);
        jsonObj->insert("CPH",ContactPhone);
        jsonObj->insert("CNA",ContactName);
        jsonObj->insert("FRT",FromType);
        jsonObj->insert("LNK",StreamLink); // link of the stream
        jsonObj->insert("SRC",BufferSource); // source ( if data was downloaded in sdcard)
        jsonObj->insert("ST",StreamType); // stream type
        jsonObj->insert("FRID",FromGroupOrChannelMemberID);
        jsonObj->insert("FROM",From);
        jsonObj->insert("DATE",Date);
        jsonObj->insert("SELF",isSelf);
        QTextStream writer(&datafile);
        QJsonDocument doc(*jsonObj);
        QString strJson(doc.toJson(QJsonDocument::Compact));
        writer << strJson;
    } catch (QException ignored) {

    }
}

QString ChaMDatabase::getNode(QString NodeType,QString NodeID,QString Key){
    // KEYS  MEMBER  ::  NAME  -  BIO  -  ID  -  PIC
    // KEYS  GROUP  ::  NAME  -  BIO  -  ID  -  PIC  -  MEMBERS  -  ADMINS  -  BOSS
    // KEYS  CHANNEL  ::  NAME  -  BIO  -  ID  -  PIC  -  MEMBERS  -  ADMINS  -  BOSS
    QString hashID = security->CHHash(NodeID);
    try{
        QFile info(MAINDIR+"/Data/Profile/"+NodeType+"/"+hashID+"/"+Key);
        if(info.exists()){
            info.open(QIODevice::ReadOnly);
            QTextStream reader(&info);
            return reader.readAll();
        }else{
            return NULL;
        }
    }catch (QException ignored){
        return NULL;
    }
}

void ChaMDatabase::insertNode(QString NodeType,QString NodeID,QString Key,QString Value){
    // KEYS MEMBER ::  NAME  -  BIO  -  ID  -  PIC
    // KEYS GROUP AND CHANNEL ::  NAME  -  BIO  -  ID  -  PIC  -  MEMBERS  -  ADMINS  -  BOSS
    try{
        QString hashID = security->CHHash(NodeID);
        QDir nodeDir(MAINDIR);
        nodeDir.mkdir("Data/Profile/"+NodeType+"/"+hashID);
        if(!Key.contains("MEMBERS") && !Key.contains("ADMINS")){
            QFile file(MAINDIR+"/Data/Profile/"+NodeType+"/"+hashID+"/"+Key);
            file.open(QIODevice::ReadWrite);
            QTextStream writer(&file);
            writer << Value;
        }else if(Key.contains("MEMBERS") || Key.contains("ADMINS")){
            QFile file(MAINDIR+"/Data/Profile/"+NodeType+"/"+hashID+"/"+Key);
            file.open(QIODevice::ReadWrite);
            QTextStream reader(&file);
            QString data = reader.readAll();
            file.close();
            file.open(QIODevice::ReadWrite);
            QTextStream writer(&file);
            writer << data + "\n" + Value;
        }


    }catch (QException ignored){}
}

bool ChaMDatabase::isNode(QString NodeType,QString NodeID){
    QString hashID = security->CHHash(NodeID);
    QDir dir(MAINDIR+"/Data/Profile/"+NodeType+"/"+hashID);
    if(dir.exists()){
        return true;
    }else{
        return false;
    }
}

void ChaMDatabase::doInfo(QString Do){
    if(Do.contains("CLEARIMG")){
        QDir dir(MAINVDIR+"/Images");
        clearFolder(dir);
    }else if(Do.contains("CLEARVDO")){
        QDir dir(MAINVDIR+"/Videos");
        clearFolder(dir);
    }else if(Do.contains("CLEARAUD")){
        QDir dir(MAINVDIR+"/Audios");
        clearFolder(dir);
    }else if(Do.contains("CLEARFILE")){
        QDir dir(MAINVDIR+"/Files");
        clearFolder(dir);
    }
}

QString ChaMDatabase::getInfo(QString Key){
    if (Key.contains("IMGSIZE")){
        QDir dir(MAINVDIR+"/Images");
        return getFolderSize(dir)+"";
    }else if(Key.contains("VDOSIZE")){
        QDir dir(MAINVDIR+"/Videos");
        return getFolderSize(dir)+"";
    }else if(Key.contains("AUDSIZE")){
        QDir dir(MAINVDIR+"/Audios");
        return getFolderSize(dir)+"";
    }else if(Key.contains("FILESIZE")){
        QDir dir(MAINVDIR+"/Files");
        return getFolderSize(dir)+"";
    }else{
        return NULL;
    }
}

long ChaMDatabase::getFolderSize(QDir folder) {
    QFileInfo info(folder.absolutePath());
    return info.size();
}

void ChaMDatabase::clearFolder(QDir folder){
    try{
        folder.removeRecursively();
    }catch (QException ignore){}
}

QString ChaMDatabase::saveFile(QString TargetType,QString TargetID,QString Time,QString FilePath){
    // return saved file path





}
