#include "chamhttp.h"
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QNetworkRequest>
#include <QByteArray>
#include <QUrl>
#include <QHttpPart>
#include <QLoggingCategory>

QNetworkAccessManager *networkaccess;

ChaMHttp::ChaMHttp(QObject *parent) : QObject(parent)
{
    networkaccess = new QNetworkAccessManager(this);
}

void ChaMHttp::onGetReply(QNetworkReply *reply){
    emit onReply(reply->readAll());
}

void ChaMHttp::onPostReply(QNetworkReply *reply){
    emit onReply(reply->readAll());
}

void ChaMHttp::GetRequest(QUrl url){
    QLoggingCategory::setFilterRules("qt.network.ssl.warning=false");
    connect(networkaccess, SIGNAL(finished(QNetworkReply*)),this, SLOT(onGetReply(QNetworkReply*)));
    QNetworkRequest request(url);
    networkaccess->get(request);
}

void ChaMHttp::PostRequest(QUrl url , QByteArray postData){
    QLoggingCategory::setFilterRules("qt.network.ssl.warning=false");
    connect(networkaccess, SIGNAL(finished(QNetworkReply*)),this, SLOT(onPostReply(QNetworkReply*)));
    QNetworkRequest request(url);
    request.setHeader(QNetworkRequest::ContentTypeHeader,"application/x-www-form-urlencoded");
    networkaccess->post(request,postData);
}

