import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.1
import Material 0.2
import Material.ListItems 0.1 as ListItem
import QtQuick.Controls.Material 2.0

Dialog {
    width: 300
    x: parent.width/2 - width/2
    y:parent.height/2 - height/2

    Image {
        x: parent.width/2 - width/2
        y:0
        id: koliber
        source: "qrc:///Data/data/KoLiBer.png"
        width: 100
        height: 100
    }

    Text{
        y:110
        wrapMode: Text.WordWrap
        width: parent.width
        height: 100
        text: "Hi guys :) . \n My nick name is KoLiBer and i'm 18 .\nChaM is an application for huge communications such as media confrencing and broadcast channels which is powered by KoLiBer"
    }



}
