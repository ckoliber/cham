import QtQuick 2.7
import QtQuick.Controls 2.0
import Material 0.2
import QtMultimedia 5.0
import Material.ListItems 0.1 as ListItem
import QtQuick.Layouts 1.3
import QtQuick.Dialogs 1.2 as Dialogs

Dialog{
    x:parent.width/2 - width/2
    y:parent.height/2 - height/2
    signal sendImage(string path);
    signal editImage(string path);
    property int saveImageState: 0
    property string capturedImagePath: ""
    Component.onCompleted: camera.stop()

    Camera {
        id: camera
        cameraState: Camera.UnloadedState
        imageProcessing.whiteBalanceMode: CameraImageProcessing.WhiteBalanceFlash
        exposure {
            exposureCompensation: -1.0
            exposureMode: Camera.ExposurePortrait
        }
        flash.mode: Camera.FlashRedEyeReduction
        imageCapture.resolution: "640x480"
        imageCapture {
            onImageCaptured: {
                imageCaptureDialogIDimageViewer.source = preview
            }
            onImageSaved: {
                capturedImagePath = path
            }
        }
    }
    id:imageCaptureDialogID
    hasActions: false
    onOpened: {
        camera.start()
        capturedImagePath = ""
    }
    onClosed: {
        camera.stop()
        camera.cameraState = Camera.UnloadedState
        if(saveImageState == 0){
            SIGNALHANDLER.removeFile(capturedImagePath,"IMG");
        }else if(saveImageState == 1){
            editImage(capturedImagePath)
        }else{
            var path = SIGNALHANDLER.saveFile(capturedImagePath,"IMG")
            sendImage(path)
        }
    }
    width: if(parent.width <= 600){parent.width-20}else if(parent.width > 600 && parent.width <= 1000){(2*parent.width)/3}else{600}
    View{
        width: parent.width
        height: camera.imageCapture.resolution.height/camera.imageCapture.resolution.width*parent.width + 50
        Image{
            id:imageCaptureDialogIDimageViewer
            y:20
            visible: if(capturedImagePath == ""){false}else{true}
            width: parent.width
            height: camera.imageCapture.resolution.height/camera.imageCapture.resolution.width*parent.width
        }

        VideoOutput {
            visible: if(capturedImagePath == ""){true}else{false}
            y:20
            width: parent.width
            height: camera.imageCapture.resolution.height/camera.imageCapture.resolution.width*parent.width
            source: camera
            focus : visible
        }

        ActionButton{
            width: 40
            height: 40
            y:camera.imageCapture.resolution.height/camera.imageCapture.resolution.width*parent.width - height/2 + 20
            x:parent.width/4 - width/2
            backgroundColor: Palette.colors[theme.primaryColor]["300"]
            Icon{
                source: "qrc:///Icons/icons/close.png"
                color: "#333"
                anchors.centerIn: parent
            }
            onClicked:{
                imageCaptureDialogID.close()
            }
        }

        ActionButton{
            id:imageCaptureDialogIDcaptureButtonID
            width: 40
            height: 40
            y:camera.imageCapture.resolution.height/camera.imageCapture.resolution.width*parent.width - height/2 + 20
            x:parent.width/2 - width/2
            backgroundColor: Palette.colors[theme.primaryColor]["300"]
            Icon{
                id:imageCaptureDialogIDcaptureButtonIDIcon
                source: if(capturedImagePath == ""){"qrc:///Icons/icons/camera-iris.png"}else{"qrc:///Icons/icons/pencil.png"}
                color: "#333"
                anchors.centerIn: parent
            }
            onClicked:if(capturedImagePath == ""){
                camera.imageCapture.capture();
                camera.stop()
            }else{
                saveImageState = 1
                imageCaptureDialogID.close()
            }
        }

        ActionButton{
            id:imageCaptureDialogIDsendButtonID
            width: 40
            height: 40
            y:camera.imageCapture.resolution.height/camera.imageCapture.resolution.width*parent.width - height/2 + 20
            x:parent.width*3/4 - width/2
            backgroundColor: Palette.colors[theme.primaryColor]["300"]
            state: if(capturedImagePath == ""){"DS"}else{"EN"}
            states: [
                State {
                    name: "EN"
                    PropertyChanges {
                        target: imageCaptureDialogIDsendButtonID
                        backgroundColor: Palette.colors[theme.primaryColor]["300"]
                        enabled: true
                    }
                },
                State {
                    name: "DS"
                    PropertyChanges {
                        target: imageCaptureDialogIDsendButtonID
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
            onClicked:if(capturedImagePath != ""){
                saveImageState = 2
                imageCaptureDialogID.close()
            }
        }

    }


}
