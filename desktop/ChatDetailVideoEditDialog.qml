import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.1
import Material 0.2
import QtMultimedia 5.5
import Material.ListItems 0.1 as ListItem
import QtQuick.Controls.Material 2.0
Dialog{
    signal sendVideo(string path)
    property int playTime: 0
    property int fromTime: 0
    property int toTime: 0
    property int videoTime: 0
    property string videoPath: ""
    property int beforeTime : 0
    property int closeState: 0
    property bool isRemovable: false

    function cutVideo(fromMilisec,toMilisec){
        // cut video and save it in that temp path
        videoPath = SIGNALHANDLER.cutMedia(videoPath,fromMilisec,toMilisec,1)
    }

    id:videoEditDialogID
    hasActions: false
    onOpened: {
        closeState = 0
        playTime = 0
        fromTime = 0
        toTime = 0
        videoEditorCutterSlider.first.value = 0
        videoEditorCutterSlider.second.value = 1
        videoEditorCutterSlider.enabled = true
        beforeTime = 0
        videoTime = 0
        videoEditorCutterButtonID.state = "EN"
        videoEditorDialogPlayButtonID.state = "PAUSE"
    }
    onClosed: {
        videoPlayer.stop()
        if(closeState == 1){
            sendVideo(videoPath)
        }else{
            // remove video temp !
            SIGNALHANDLER.removeFile(videoPath,"VDO")
        }
    }

    View{width: parent.width;height: 20}

    View{
        backgroundColor: Palette.colors[theme.primaryColor]["100"]
        radius: 3
        elevation: 2
        width: parent.width
        height: 210

        MediaPlayer {
            id: videoPlayer
            source: videoPath
            onDurationChanged:{
                videoTime = duration/1000
                toTime = videoTime
            }
        }

        VideoOutput {
            width: parent.width
            height: 150
            source: videoPlayer
        }

        Slider{
            color: Palette.colors[theme.primaryColor]["500"]
            x:5
            y:150
            width: parent.width - 10
            minimumValue: 0
            maximumValue: videoTime
            value: playTime
            onValueChanged:{
                if(value-beforeTime > 1 || beforeTime - value > 1){
                    playTime = value;
                    videoPlayer.seek(playTime*1000)
                }
                beforeTime = value
            }
        }

        Label{
            x:10
            y:190 - height/2
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
            id: videoEditorPlayerTimerID
            interval: 1000
            onTriggered:
                if(playTime <= videoTime){
                    playTime++
                }else{
                    videoEditorDialogPlayButtonID.state = "PAUSE"
                    playTime = 0
                    videoPlayer.seek(0)
                }
        }

        ActionButton{
            id:videoEditorDialogPlayButtonID
            x:parent.width - width - 10
            y:190 - height/2
            width: 25
            height: 25
            backgroundColor: Palette.colors[theme.primaryColor]["300"]
            Icon{
                id:videoEditPlayerButtonIconID
                source: "qrc:///Icons/icons/play.png"
                color: "#333"
                anchors.centerIn: parent
            }
            state: "PAUSE"
            states: [
                State {
                    name: "PLAY"
                    PropertyChanges {
                        target: videoEditPlayerButtonIconID
                        source: "qrc:///Icons/icons/pause.png"
                    }
                    StateChangeScript{
                        script: {
                            videoEditorPlayerTimerID.start()
                            videoPlayer.play()
                        }
                    }
                },
                State {
                    name: "PAUSE"
                    PropertyChanges {
                        target: videoEditPlayerButtonIconID
                        source: "qrc:///Icons/icons/play.png"
                    }
                    StateChangeScript{
                        script: {
                            videoEditorPlayerTimerID.stop()
                            videoPlayer.pause()
                        }
                    }
                }
            ]

            onClicked: if(state == "PLAY"){state = "PAUSE"}else{state = "PLAY"}
        }

    }

    View{
        backgroundColor: Palette.colors[theme.primaryColor]["100"]
        radius: 3
        elevation: 2
        width: parent.width
        height: 110

        RangeSlider{
            id:videoEditorCutterSlider
            y:30
            Material.accent: Palette.colors[theme.primaryColor]["500"]
            width: parent.width
            first.value: 0
            second.value: 1
            first.onPositionChanged: fromTime = (first.position)*videoTime
            second.onPositionChanged: toTime = (second.position)*videoTime
        }

        Label{
            x:10
            y:70 - height/2
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
            y:90 - height/2
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
            id:videoEditorCutterButtonID
            x:parent.width - width - 10
            y:90 - height/2
            width: 25
            height: 25
            state:"EN"
            Icon{
                source: "qrc:///Icons/icons/cut.png"
                color: "#333"
                anchors.centerIn: parent
                scale: 0.7
            }
            onClicked: {
                if(videoEditorCutterButtonID.state == "EN"){
                    videoEditorCutterButtonID.state = "DS"
                    videoEditorCutterSlider.enabled = false
                    cutVideo((videoEditorCutterSlider.first.position)*videoTime*1000,(videoEditorCutterSlider.second.position)*videoTime*1000)
                }
            }

            states: [
                State {
                    name: "EN"
                    PropertyChanges {
                        target: videoEditorCutterButtonID
                        backgroundColor:Palette.colors[theme.primaryColor]["300"]
                        enabled: true


                    }
                },
                State {
                    name: "DS"
                    PropertyChanges {
                        target: videoEditorCutterButtonID
                        backgroundColor :"grey"
                        enabled:false


                    }
                }
            ]

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
            onClicked:videoEditDialogID.close()
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
            onClicked:{closeState = 1;videoEditDialogID.close()}
        }

    }

}
