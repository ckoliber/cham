import QtQuick 2.7
import QtQuick.Controls 2.0
import Material 0.2
import QtMultimedia 5.0
import Material.ListItems 0.1 as ListItem
import QtQuick.Layouts 1.3
import QtQuick.Dialogs 1.2 as Dialogs

Dialog{
    signal sendFile(string path);
    property string fileName: ""
    property string filePath: ""
    property string fileSize: ""
    property int closeState: 0

    contentMargins: 0
    id:fileShowDialogID
    hasActions: false
    onOpened: {
        closeState = 0;
        var size = SIGNALHANDLER.getFileSize(filePath)
        if(size / (1024*1024*1024) >= 1){
            // Gigabyte
            fileSize = (size/(1024*1024*1024)).toFixed(2)+" GB";
        }else{
            if(size / (1024*1024) >= 1){
                // Megabyte
                fileSize = (size/(1024*1024)).toFixed(2)+" MB";
            }else{
                if(size / 1024 >= 1){
                    // Kilobyte
                    fileSize = (size/1024).toFixed(2)+" KB";
                }else{
                    // Byte
                    fileSize = size.toFixed(2)+" B";
                }
            }
        }
        fileSize = fSize
        fileName = SIGNALHANDLER.getFileName(filePath)
    }
    onClosed: {
        if(closeState == 1){
            // save file !!!
            sendFile(filePath)
        }
    }

    View{
        width: parent.width
        height: 60

        View{
            width: 50
            height: 50
            x:5
            y:5
            elevation: 2
            radius: 50
            backgroundColor: Palette.colors[theme.primaryColor]["400"]

            Icon {
                anchors.centerIn: parent
                id: fileIcon
                source: "qrc:///Icons/icons/filedata.png"
            }
        }

        View{
            radius: 3
            elevation: 1
            width: parent.width - 70
            y:5
            x:60
            height: 50
            backgroundColor: Palette.colors[theme.primaryColor]["200"]
            Text{
                width: parent.width - 5
                elide: Text.ElideRight
                x:5
                y:parent.height/3 - height/2
                text:"<b>File name :</b> " + fileName
            }

            Text{
                width: parent.width - 5
                elide: Text.ElideRight
                x:5
                y:parent.height*2/3 - height/2
                text:"<b>File size :</b> " + fileSize
            }


        }








    }

    View{
        width: parent.width
        height: 50
        ActionButton{
            x:parent.width/3 - width/2
            width: 40
            height: 40
            backgroundColor: Palette.colors[theme.primaryColor]["300"]
            Icon{
                source: "qrc:///Icons/icons/close.png"
                color: "#333"
                anchors.centerIn: parent
            }
            onClicked:fileShowDialogID.close()
        }


        ActionButton{
            x:parent.width*2/3 - width/2
            width: 40
            height: 40
            backgroundColor: Palette.colors[theme.primaryColor]["300"]
            Icon{
                source: "qrc:///Icons/icons/send.png"
                color: "#333"
                anchors.centerIn: parent
            }
            onClicked:{closeState = 1;fileShowDialogID.close()}
        }

    }

}
