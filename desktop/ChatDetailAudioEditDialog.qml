import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.1
import Material 0.2
import QtMultimedia 5.0
import Material.ListItems 0.1 as ListItem
import QtQuick.Controls.Material 2.0

Dialog{
    signal sendAudio(string path)
    property var buffers: []
    property int playTime: 0
    property int fromTime: 0
    property int toTime: 0
    property int beforeTime: 0
    property string audioPath: ""
    property int closeState: 0
    property int audioTime: 0
    property bool isRemovable: false

    function cutAudio(fromMilisec,toMilisec){
        // cut audio and save it in that temp path
        // new path -> path
        audioPath = SIGNALHANDLER.cutMedia(audioPath,fromMilisec,toMilisec,0)
    }

    id:audioEditDialogID
    hasActions: false
    onOpened: {
        if(!isRemovable){
            audioPath = "file://"+SIGNALHANDLER.copyFile(audioPath,"AUD");
            if(audioPath == ""){audioEditDialogID.close();snackbar.open("Some errors was occured !")}
        }
        buffers = []
        playTime = 0
        fromTime = 0
        toTime = 0
        beforeTime = 0
        closeState = 0
        audioTime = 0
        audioEditDialogIDcutButtonID.state = "EN"
        audioEditorPlayButtonID.state = "PAUSE"
        audioEditorCutterSlider.enabled = true
        audioEditorCutterSlider.first.value = 0
        audioEditorCutterSlider.second.value = 1
        SIGNALHANDLER.processWave(audioPath)
    }
    onClosed: {
        audioPlayer.stop()
        if(closeState == 1){
            // save audio -> cut !!!
            sendAudio(audioPath)
        }else{
            SIGNALHANDLER.removeFile(audioPath,"AUD")
            // remove audio temp file !
        }
    }

    Connections{
        target: SIGNALHANDLER
        onWaveBuffer:{
            buffers.push(mean)
        }
        onWaveEnd:{
            canvas.requestPaint();
        }
    }

    View{width: parent.width;height: 20}

    View{
        backgroundColor: Palette.colors[theme.primaryColor]["100"]
        radius: 3
        elevation: 2
        width: parent.width
        height: 170
        Label{
            text: "Cutter"
            font.weight: Font.Bold
            x:10
            y: 10
        }
        View{
            y:30
            id:audioWaveViewer
            width: parent.width - 20
            x:10
            height: 50
            elevation:3
            Canvas{
                id:canvas
                anchors.fill: parent
                onPaint: {
                    var ctx = getContext('2d');
                    for(var a = 0 ; a < buffers.length ; a++){
                        ctx.strokeStyle = Palette.colors[theme.primaryColor]["500"]
                        ctx.beginPath()
                        ctx.moveTo(canvas.width / buffers.length * a , canvas.height/30000*(15000 + buffers[a]));
                        ctx.lineTo(canvas.width / buffers.length * (a+1) , canvas.height/30000*(15000 + buffers[a+1]));
                        ctx.stroke();
                    }
                }
            }

            Rectangle{
                height: parent.height
                width : (audioEditorCutterSlider.second.position-audioEditorCutterSlider.first.position)*canvas.width
                x : (audioEditorCutterSlider.first.position)*canvas.width
                color:"#34000000"
            }

        }

        RangeSlider{
            id:audioEditorCutterSlider
            y:80
            Material.accent: Palette.colors[theme.primaryColor]["500"]
            width: parent.width
            first.value: 0
            second.value: 1
            first.onPositionChanged: fromTime = (first.position)*audioTime
            second.onPositionChanged: toTime = (second.position)*audioTime
        }

        Label{
            x:10
            y:120 - height/2
            text:{
                var hour = parseInt(fromTime % (60*60*24) / (60*60));
                var min = parseInt(fromTime % (60*60) / (60));
                var sec = parseInt(fromTime % (60));
                var time = "<b>From time :</b> ";
                if(hour < 10){time += "0"+hour}else{time += hour}
                time += ":"
                if(min < 10){time += "0"+min}else{time += min}
                time += ":"
                if(sec < 10){time += "0"+sec}else{time += sec}
                time
            }
        }
        Label{
            x:10
            y:140 - height/2
            text:{
                var hour = parseInt(toTime % (60*60*24) / (60*60));
                var min = parseInt(toTime % (60*60) / (60));
                var sec = parseInt(toTime % (60));
                var time = "<b>To time :</b> ";
                if(hour < 10){time += "0"+hour}else{time += hour}
                time += ":"
                if(min < 10){time += "0"+min}else{time += min}
                time += ":"
                if(sec < 10){time += "0"+sec}else{time += sec}
                time
            }
        }

        ActionButton{
            id:audioEditDialogIDcutButtonID
            x:parent.width - width - 10
            y:140 - height/2
            width: 25
            height: 25
            backgroundColor: Palette.colors[theme.primaryColor]["300"]
            Icon{
                source: "qrc:///Icons/icons/cut.png"
                color: "#333"
                anchors.centerIn: parent
                scale: 0.7
            }
            onClicked: {
                if(audioEditDialogIDcutButtonID.state == "EN"){
                    audioEditDialogIDcutButtonID.state = "DS"
                    audioEditorCutterSlider.enabled = false
                    cutAudio((audioEditorCutterSlider.first.position)*audioTime*1000,(audioEditorCutterSlider.second.position)*audioTime*1000)
                }
            }

            state: "EN"
            states: [
                State {
                    name: "EN"
                    PropertyChanges {
                        target: audioEditDialogIDcutButtonID
                        backgroundColor: Palette.colors[theme.primaryColor]["300"]
                        enabled: true
                    }
                },
                State {
                    name: "DS"
                    PropertyChanges {
                        target: audioEditDialogIDcutButtonID
                        backgroundColor: "grey"
                        enabled: false
                    }
                }

            ]

        }

    }

    View{
        backgroundColor: Palette.colors[theme.primaryColor]["100"]
        radius: 3
        elevation: 2
        width: parent.width
        height: 90

        MediaPlayer{
            source: audioPath
            id:audioPlayer
            onDurationChanged:{audioTime = duration/1000;toTime = duration/1000}
        }

        Label{
            text: "Player"
            font.weight: Font.Bold
            x:10
            y: 10
        }

        Slider{
            color: Palette.colors[theme.primaryColor]["500"]
            x:5
            y:30
            width: parent.width - 10
            minimumValue: 0
            maximumValue: audioTime
            value: playTime
            onValueChanged: {
                if(value - beforeTime > 1 || beforeTime - value > 1){
                    // seek audio to value
                    audioPlayer.seek(value*1000)
                    playTime = value

                }
                beforeTime = value
            }
        }

        Label{
            x:10
            y:70 - height/2
            text:{
                var hour = parseInt(playTime % (60*60*24) / (60*60));
                var min = parseInt(playTime % (60*60) / (60));
                var sec = parseInt(playTime % (60));
                var time = "<b>Play time :</b> ";
                if(hour < 10){time += "0"+hour}else{time += hour}
                time += ":"
                if(min < 10){time += "0"+min}else{time += min}
                time += ":"
                if(sec < 10){time += "0"+sec}else{time += sec}
                time
            }
        }

        Timer{
            repeat: true
            id: audioEditorPlayerTimerID
            interval: 1000
            onTriggered:
                if(playTime <= audioTime){
                    playTime++
                }else{
                    audioEditorPlayButtonID.state = "PAUSE"
                    playTime = 0
                }
        }

        ActionButton{
            x:parent.width - width - 10
            y:70 - height/2
            width: 25
            height: 25
            id:audioEditorPlayButtonID
            backgroundColor: Palette.colors[theme.primaryColor]["300"]
            Icon{
                id:audioEditPlayerButtonIconID
                source: "qrc:///Icons/icons/play.png"
                color: "#333"
                anchors.centerIn: parent
            }
            state: "PAUSE"
            states: [
                State {
                    name: "PLAY"
                    PropertyChanges {
                        target: audioEditPlayerButtonIconID
                        source: "qrc:///Icons/icons/pause.png"
                    }
                    StateChangeScript{
                        script: {
                            audioEditorPlayerTimerID.start()
                            audioPlayer.play()
                        }
                    }
                },
                State {
                    name: "PAUSE"
                    PropertyChanges {
                        target: audioEditPlayerButtonIconID
                        source: "qrc:///Icons/icons/play.png"
                    }
                    StateChangeScript{
                        script: {
                            audioEditorPlayerTimerID.stop()
                            audioPlayer.pause()
                        }
                    }
                }
            ]

            onClicked: if(state == "PLAY"){state = "PAUSE"}else{state = "PLAY"}



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
            onClicked:audioEditDialogID.close()
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
            onClicked:{
                closeState = 1
                audioEditDialogID.close()
            }
        }

    }

}
