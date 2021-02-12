import QtQuick 2.7
import QtQuick.Controls 2.0
import Material 0.2
import QtMultimedia 5.0
import Material.ListItems 0.1 as ListItem
import QtQuick.Layouts 1.3
import QtQuick.Dialogs 1.2 as Dialogs

Dialog{

    signal sendVideo(string path);
    signal editVideo(string path);
    property int videoRecTime : 0
    property int saveVideoState: 0
    property string recordedVideoPath: ""
    Component.onCompleted: camera.stop()
    Camera {
        id: camera
        cameraState: Camera.UnloadedState
        captureMode: Camera.CaptureVideo
        videoRecorder.audioChannels: 2
        videoRecorder.audioCodec: "audio/mpeg, mpegversion=(int)4"
        videoRecorder.videoCodec: "video/x-h264"
        videoRecorder.mediaContainer: "video/mp4"
        videoRecorder.resolution: "640x480"
        videoRecorder.frameRate: 30
        videoRecorder.onRecorderStateChanged: {
           if (camera.videoRecorder.recorderStatus == CameraRecorder.RecordingStatus) {
               recordedVideoPath = camera.videoRecorder.outputLocation
           }
        }
    }
    id:videoRecordDialogID
    hasActions: false

    function startVideoRecord(){
        camera.videoRecorder.record()
        videorecordertimer.start()
    }

    function stopVideoRecord(){
        camera.videoRecorder.stop()
        videorecordertimer.stop()
    }

    onOpened: {
        camera.start()
        videoRecTime = 0
        saveVideoState = 0
        recordedVideoPath = ""
        videoRecordDialogIDrecordButtonIDIcon.source = "qrc:///Icons/icons/record.png"
    }

    onClosed:{
        videorecordertimer.stop()
        camera.stop()
        camera.cameraState = Camera.UnloadedState

        var tempPath = recordedVideoPath
        if(saveVideoState == 0 ){
            SIGNALHANDLER.removeFile(tempPath,"VDO")
        }else if(saveVideoState == 1){
            editVideo(tempPath)
        }else{
            var path = SIGNALHANDLER.saveFile(tempPath,"VDO")
            sendVideo(path)
        }
    }

    width: if(parent.width <= 600){parent.width-20}else if(parent.width > 600 && parent.width <= 1000){(2*parent.width)/3}else{600}
    View{
        width: parent.width
        height: camera.videoRecorder.resolution.height/camera.videoRecorder.resolution.width*parent.width + 80

        Timer{
            id:videorecordertimer
            interval: 1000
            repeat: true
            running: false
            onTriggered: {
                videoRecTime++;
            }
        }
        Label{
            text:{
                var hour = parseInt(videoRecTime % (60*60*24) / (60*60));
                var min = parseInt(videoRecTime % (60*60) / (60));
                var sec = parseInt(videoRecTime % (60));
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
            y:10
        }

        VideoOutput {
            id:videRecordDialogIDvideoViewer
            y:50
            width: parent.width
            height: camera.videoRecorder.resolution.height/camera.videoRecorder.resolution.width*parent.width
            source: camera
            focus : visible
        }

        ActionButton{
            width: 40
            height: 40
            y:videRecordDialogIDvideoViewer.height + 50 - height/2
            x:parent.width/4 - width/2
            backgroundColor: Palette.colors[theme.primaryColor]["300"]
            Icon{
                source: "qrc:///Icons/icons/close.png"
                color: "#333"
                anchors.centerIn: parent
            }
            onClicked:{
                videoRecordDialogID.close()
            }
        }

        ActionButton{
            id:videoRecordDialogIDrecordButtonID
            width: 40
            height: 40
            y:videRecordDialogIDvideoViewer.height + 50 - height/2
            x:parent.width/2 - width/2
            backgroundColor: Palette.colors[theme.primaryColor]["300"]
            Icon{
                id:videoRecordDialogIDrecordButtonIDIcon
                source: "qrc:///Icons/icons/record.png"
                color: "#333"
                anchors.centerIn: parent
            }
            onClicked:if(videoRecordDialogIDrecordButtonIDIcon.source == "qrc:///Icons/icons/record.png"){
                videoRecordDialogIDrecordButtonIDIcon.source = "qrc:///Icons/icons/stop.png";
                startVideoRecord()

            }else if(videoRecordDialogIDrecordButtonIDIcon.source == "qrc:///Icons/icons/stop.png"){
                videoRecordDialogIDrecordButtonIDIcon.source = "qrc:///Icons/icons/pencil.png";
                stopVideoRecord()
            }else{
                saveVideoState = 1
                videoRecordDialogID.close()
            }
        }

        ActionButton{
            id:videoRecordDialogIDsendButtonID
            width: 40
            height: 40
            y:videRecordDialogIDvideoViewer.height + 50 - height/2
            x:parent.width*3/4 - width/2
            backgroundColor: Palette.colors[theme.primaryColor]["300"]
            state: if(videoRecTime > 0){"EN"}else{"DS"}
            states: [
                State {
                    name: "EN"
                    PropertyChanges {
                        target: videoRecordDialogIDsendButtonID
                        backgroundColor: Palette.colors[theme.primaryColor]["300"]
                        enabled: true
                    }
                },
                State {
                    name: "DS"
                    PropertyChanges {
                        target: videoRecordDialogIDsendButtonID
                        backgroundColor: "grey"
                        enabled: false
                    }
                }

            ]

            Icon{
                source: "qrc:///Icons/icons/send.png"
                color: "#333"
                anchors.centerIn: parent
            }

            onClicked:if(videoRecTime > 0){
                          saveVideoState = 2
                          videoRecordDialogID.close()
            }

        }

    }

}
