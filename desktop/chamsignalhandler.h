#ifndef CHAMSIGNALHANDLER_H
#define CHAMSIGNALHANDLER_H
#include <QQmlApplicationEngine>
#include <QDebug>
#include <QObject>
#include <QJsonObject>
#include <QJsonDocument>
#include <QException>
#include <QString>
#include <QHostAddress>
#include <QAudioRecorder>
#include <QAudioEncoderSettings>
#include <QAudioBuffer>
#include <QMultimedia>
#include <QAudioDecoder>
#include <QMediaPlayer>
#include <QDir>
#include <QStandardPaths>
#include <QImage>
#include <QRect>
#include <QAudioProbe>
#include "chamdatabase.h"
#include "chamsecurity.h"
#include "libavutil/mathematics.h"
#include "libavformat/avformat.h"
#include "libswscale/swscale.h"

class ChaMSignalHandler : public QObject
{
    Q_OBJECT
public:
    explicit ChaMSignalHandler();
    Q_INVOKABLE void loadMainList();
    Q_INVOKABLE void loadItemDetails(QString TargetID,QString TargetType);
    Q_INVOKABLE void startRecordAudio();
    Q_INVOKABLE void resumeRecordAudio();
    Q_INVOKABLE void pauseRecordAudio();
    Q_INVOKABLE QString stopRecordAudio();
    Q_INVOKABLE void processWave(QString path);
    Q_INVOKABLE QString cropImage(QString path , int x , int y , int width , int height);
    Q_INVOKABLE int getFileFormat(QString path);
    Q_INVOKABLE qint64 getFileSize(QString path);
    Q_INVOKABLE QString getFileName(QString path);
    Q_INVOKABLE void removeFile(QString path,QString type);
    Q_INVOKABLE QString saveFile(QString path,QString type);
    Q_INVOKABLE QString copyFile(QString path,QString type);
    QString getFileFormatString(QString path);
    Q_INVOKABLE QString recentEmojies(QString emojiUnicode);
    Q_INVOKABLE QString cutMedia(QString path,quint64 startMil,quint64 endMil,int mediaType);
    Q_INVOKABLE QStringList loadUser(QString TargetType,QString TargetID);

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
                 QString StreamType,
                 bool isSelf,
                 int MessagesCount);


    void messageDetail(QString DataType,
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
                              QString StreamType,
                              bool isSelf);
    void setTargetPage(QString TargetID,QString TargetType);
    void clearDetail();
    void call(bool isSelf,QString CallType,QString FromID,QHostAddress FromIP,quint16 FromPort);
    void waveBuffer(int mean);
    void waveEnd();

public slots:
    void audioDecoderReadBuffer();
    void audioDecoderEnd();


private:
    QAudioDecoder *audioDecoder;
    ChaMDatabase *database;
    ChaMSecurity *security;
    QAudioRecorder *audioRecorder;

};

#endif // CHAMSIGNALHANDLER_H
