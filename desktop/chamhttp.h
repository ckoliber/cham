#ifndef CHAMHTTP_H
#define CHAMHTTP_H

#include <QObject>
#include <QNetworkReply>
#include <QByteArray>
#include <QUrl>

class ChaMHttp : public QObject
{
    Q_OBJECT
public:
    explicit ChaMHttp(QObject *parent = 0);
    void GetRequest(QUrl url);
    void PostRequest(QUrl url,QByteArray postData);

signals:
    void onReply(QString reply);

public slots:
    void onGetReply(QNetworkReply *reply);
    void onPostReply(QNetworkReply *reply);

};

#endif // CHAMHTTP_H
