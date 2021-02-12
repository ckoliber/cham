import QtQuick 2.7
import QtQuick.Controls 2.0
import Material 0.2
import QtWebEngine 1.0
import Material.ListItems 0.1 as ListItem
import QtQuick.Layouts 1.3
import QtQuick.Dialogs 1.2 as Dialogs

Dialog{
    property var locationLat: 6.906105188659279
    property var locationLong: 79.85339641571045
    contentMargins: 0
    id:locationShowDialogID

    hasActions: false
    onOpened: {
        var locationUrl = "http://www.openstreetmap.org/export/embed.html?bbox="+(locationLong-0.005)+"%2C"+(locationLat-0.005)+"%2C"+(locationLong+0.005)+"%2C"+(locationLat+0.005)+"&layer=mapnik&marker="+locationLat+"%2C"+locationLong;
        mapLoaderID.url = locationUrl;
    }
    onClosed: {
        mapLoaderID.stop()
        determinateProgress.value = 0
    }

    View{
        width: parent.width
        height: 300
        WebEngineView{
            id:mapLoaderID
            anchors.fill: parent
            onLoadProgressChanged: determinateProgress.value = loadProgress
            onLoadingChanged: if(loading){loaderRectangleID.visible = true}else{loaderRectangleID.visible = false}
        }

        Rectangle{
            anchors.fill: parent
            id:loaderRectangleID
            z:50
            ProgressCircle {
                anchors.centerIn: parent
                id: determinateProgress
                Layout.alignment: Qt.AlignCenter
                width: dp(64)
                height: dp(64)
                indeterminate: false
                minimumValue: 0
                maximumValue: 100

                SequentialAnimation on value {
                    running: true
                    loops: NumberAnimation.Infinite

                    NumberAnimation {
                        duration: 3000
                        from: determinateProgress.minimumValue
                        to: determinateProgress.maximumValue
                    }

                    PauseAnimation { duration: 1000 }
                }

                Label {
                    anchors.centerIn: parent
                    text: Math.round(determinateProgress.value) + "%"
                }
            }



        }


    }




}
