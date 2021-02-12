#include "chamdate.h"
#include <iostream>
#include <chrono>
#include <QDebug>
#include <QDateTime>
#include <iomanip>

ChaMDate::ChaMDate(QObject *parent) : QObject(parent)
{
    dateTime = new QDateTime;
}

QString ChaMDate::getDate(){
    std::chrono::time_point<std::chrono::system_clock> now = std::chrono::system_clock::now();
    auto duration = now.time_since_epoch();
    auto micro = std::chrono::duration_cast<std::chrono::microseconds>(duration).count();
    QString date = QString::number(dateTime->currentDateTime().date().year()) + "-" +
            QString::number(dateTime->currentDateTime().date().month()) + "-" +
            QString::number(dateTime->currentDateTime().date().day()) + "-" +
            QString::number(dateTime->currentDateTime().time().hour()) + "-" +
            QString::number(dateTime->currentDateTime().time().minute()) + "-" +
            QString::number(dateTime->currentDateTime().time().second()) + "-" +
            QString::number(micro%1000000);

    return date;
}
