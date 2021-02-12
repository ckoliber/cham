#ifndef CHAMDATE_H
#define CHAMDATE_H

#include <QObject>
#include <QDateTime>

class ChaMDate : public QObject
{
    Q_OBJECT
public:
    explicit ChaMDate(QObject *parent = 0);
    Q_INVOKABLE QString getDate();

signals:

public slots:

private:
    QDateTime *dateTime;
};

#endif // CHAMDATE_H
