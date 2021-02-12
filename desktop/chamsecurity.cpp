#include "chamsecurity.h"
#include <QCryptographicHash>
#include <QByteArray>
#include <QSslCipher>

ChaMSecurity::ChaMSecurity(QObject *parent) : QObject(parent)
{

}

QString ChaMSecurity::CHHash(QString data){
    QByteArray hashdata(data.toUtf8());
    hashdata = QCryptographicHash::hash(hashdata , QCryptographicHash:: Md5).toHex();
    hashdata = QCryptographicHash::hash(hashdata , QCryptographicHash:: Sha1).toHex();
    hashdata = QCryptographicHash::hash(hashdata , QCryptographicHash:: Sha512).toHex();
    return QString(hashdata);
}

QString ChaMSecurity::CHMD5Hash(QString data){
    QByteArray hashdata(data.toUtf8());
    hashdata = QCryptographicHash::hash(hashdata , QCryptographicHash:: Md5).toHex();
    return QString(hashdata);
}


QString ChaMSecurity::CHBase64Encode(QString data){
    QByteArray bytearray(data.toUtf8());
    return QString(bytearray.toBase64());
}

QString ChaMSecurity::CHBase64Decode(QString base){
    QByteArray bytearray(base.toUtf8());
    return QString ( QByteArray::fromBase64(bytearray) );
}

QString ChaMSecurity::CHEncrypt(QString data, QString key){

    return NULL;

}

QString ChaMSecurity::CHDecrypt(QString cipher, QString key){
    return NULL;

}
