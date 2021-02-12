import QtQuick 2.7
import QtQuick.Controls 2.0
import Material 0.2
import QtMultimedia 5.0
import Material.ListItems 0.1 as ListItem
import QtQuick.Layouts 1.3
import QtQuick.Dialogs 1.2 as Dialogs

Dialog{

    signal sendAudio(string path);
    signal editAudio(string path);
    property int audioRecTime : 0
    property int saveAudioState: 0 // 0 -> delete , 1 -> edit , 2 -> send

    onOpened:{
        audiorecorddialogrecordbutton.source = "qrc:///Icons/icons/record.png"
        audioRecTime = 0
        saveAudioState = 0
        SIGNALHANDLER.startRecordAudio()
    }
    onClosed:{
        audiorecordertimer.running = false
        var tempPath = SIGNALHANDLER.stopRecordAudio()
        if(saveAudioState == 0 ){
            SIGNALHANDLER.removeFile("","AUD")
        }else if(saveAudioState == 1){
            editAudio(tempPath)
        }else{
            var path = SIGNALHANDLER.saveFile("","AUD")
            sendAudio(path)
        }
    }

    function startAudioRecord(){
        SIGNALHANDLER.resumeRecordAudio()
        audiorecordertimer.start()
    }

    function pauseAudioRecord(){
        audiorecordertimer.stop()
        SIGNALHANDLER.pauseRecordAudio()
    }

    contentMargins: 0
    id:audiorecordID
    hasActions: false
    View{
        width: parent.width
        height: 150
        Timer{
            id:audiorecordertimer
            interval: 1000
            repeat: true
            running: false
            onTriggered: {
                audioRecTime++;
            }
        }
        Label{
            text:{
                var hour = parseInt(audioRecTime % (60*60*24) / (60*60));
                var min = parseInt(audioRecTime % (60*60) / (60));
                var sec = parseInt(audioRecTime % (60));
                var time = "";
                if(hour < 10){time += "0"+hour}else{time += hour}
                time += ":"
                if(min < 10){time += "0"+min}else{time += min}
                time += ":"
                if(sec < 10){time += "0"+sec}else{time += sec}
                time
            }
            font.pixelSize: 25
            x:parent.width/2 - width/2
            y:20
        }



        ActionButton{
            width: 40
            height: 40
            y:80
            x:parent.width/5 - width/2
            backgroundColor: Palette.colors[theme.primaryColor]["300"]
            Icon{
                source: "qrc:///Icons/icons/close.png"
                color: "#333"
                anchors.centerIn: parent
            }
            onClicked: {
                audiorecordID.close()
            }
        }

        ActionButton{
            width: 40
            height: 40
            y:80
            x:parent.width*2/5 - width/2
            backgroundColor: Palette.colors[theme.primaryColor]["300"]
            Icon{
                id:audiorecorddialogrecordbutton
                source: "qrc:///Icons/icons/record.png"
                color: "#333"
                anchors.centerIn: parent
            }
            onClicked:if(audiorecorddialogrecordbutton.source == "qrc:///Icons/icons/record.png"){audiorecorddialogrecordbutton.source = "qrc:///Icons/icons/pause.png";startAudioRecord()}else{audiorecorddialogrecordbutton.source = "qrc:///Icons/icons/record.png";pauseAudioRecord()}
        }

        ActionButton{
            id:audioRecordDialogIDeditButtonID
            width: 40
            height: 40
            y:80
            x:parent.width*3/5 - width/2
            backgroundColor: Palette.colors[theme.primaryColor]["300"]

            state: if(audioRecTime > 0){"EN"}else{"DS"}
            states: [
                State {
                    name: "EN"
                    PropertyChanges {
                        target: audioRecordDialogIDeditButtonID
                        backgroundColor: Palette.colors[theme.primaryColor]["300"]
                        enabled: true
                    }
                },
                State {
                    name: "DS"
                    PropertyChanges {
                        target: audioRecordDialogIDeditButtonID
                        backgroundColor: "grey"
                        enabled: false
                    }
                }

            ]

            Icon{
                source: "qrc:///Icons/icons/pencil.png"
                color: "#333"
                anchors.centerIn: parent
            }
            onClicked: {
                saveAudioState = 1
                audiorecordID.close()
            }
        }

        ActionButton{
            id:audioRecordDialogIDsendButtonID
            width: 40
            height: 40
            y:80
            x:parent.width*4/5 - width/2
            state: if(audioRecTime > 0){"EN"}else{"DS"}
            states: [
                State {
                    name: "EN"
                    PropertyChanges {
                        target: audioRecordDialogIDsendButtonID
                        backgroundColor: Palette.colors[theme.primaryColor]["300"]
                        enabled: true
                    }
                },
                State {
                    name: "DS"
                    PropertyChanges {
                        target: audioRecordDialogIDsendButtonID
                        backgroundColor: "grey"
                        enabled: false
                    }
                }
            ]
            backgroundColor: Palette.colors[theme.primaryColor]["300"]
            Icon{
                source: "qrc:///Icons/icons/send.png"
                color: "#333"
                anchors.centerIn: parent
            }
            onClicked: {
                saveAudioState = 2
                audiorecordID.close()
            }
        }


    }
}
