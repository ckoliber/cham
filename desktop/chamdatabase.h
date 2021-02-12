#ifndef CHAMDATABASE_H
#define CHAMDATABASE_H

#include <QObject>
#include <QFile>
#include <QDir>

class ChaMDatabase : public QObject
{
    Q_OBJECT
public:
    explicit ChaMDatabase(QObject *parent = 0);

    void initDirs();
    void initClient(QString Name,QString LastSeen,QString Picture,QString Phone,QString Bio,QString ID, QString CCode,QString SCode,QString BoolSetup,QString Blocks);
    void initSetting(QString Theme,QString Language,QString AutoUpdate, QString MDAutoDownloadVideo,QString MDAutoDownloadImage,QString MDAutoDownloadAudio,QString MWAutoDownloadVideo,QString MWAutoDownloadImage,QString MWAutoDownloadAudio,QString ChatBackground,QString ListType,QString MessagesNotify,QString MessagesNotifyTone,QString MessagesNotifyVibrate,QString MessagesNotifyLEDColor,QString GPCHNotify,QString GPCHNotifyTone,QString GPCHNotifyVibrate,QString GPCHNotifyLEDColor,QString CallNotify,QString CallNotifyRingtone,QString CallNotifyVibrate,QString DataCrypto);
    QString getClient(QString Item);
    QString getSetting(QString Item);
    void removeClient(QString Key,QString ItemPos);
    void setClient(QString Item,QString Value);
    void setSetting(QString Item,QString Value);
    int getNodeLength(QString NodeType);
    int getNodeMessagesLength(QString NodeType,int Node);
    QString getNodeMessage(QString NodeType,int Node,int Message);
    QString getNodeMessageByNodeName(QString NodeType,QString Node,int Message);
    int getNodeMessagesLengthByNodeName(QString NodeType,QString Node);
    void insertNewMessage(QString DataType,
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
                          QString isSelf);
    QString getNode(QString NodeType,QString NodeID,QString Key);
    void insertNode(QString NodeType,QString NodeID,QString Key,QString Value);
    bool isNode(QString NodeType,QString NodeID);
    void doInfo(QString Do);
    QString getInfo(QString Key);
    long getFolderSize(QDir folder);
    void clearFolder(QDir folder);
    QString saveFile(QString TargetType,QString TargetID,QString Time,QString FilePath);
signals:

public slots:
};

#endif // CHAMDATABASE_H
