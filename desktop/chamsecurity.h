#ifndef CHAMSECURITY_H
#define CHAMSECURITY_H

#include <QObject>

class ChaMSecurity : public QObject
{
    Q_OBJECT
public:
    explicit ChaMSecurity(QObject *parent = 0);
    QString CHHash(QString data);
    QString CHMD5Hash(QString data);
    QString CHBase64Encode(QString data);
    QString CHBase64Decode(QString base);
    QString CHEncrypt(QString data,QString key);
    QString CHDecrypt(QString cipher,QString key);

signals:

public slots:
};

#endif // CHAMSECURITY_H
