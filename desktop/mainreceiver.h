#ifndef MAINRECEIVER_H
#define MAINRECEIVER_H

#include <QObject>
#include <QQmlApplicationEngine>
#include <QNetworkReply>

class MainReceiver : public QObject
{
    Q_OBJECT
public:
    explicit MainReceiver(QQmlApplicationEngine *engineinp);

signals:

public slots:
   void slotGetCountries();
   void slotSendCodeReply(QString reply);
   void slotSendCode(QString code,QString phone,QString ccode);
   void slotCurrentCountry(QString reply);

};

#endif // MAINRECEIVER_H
