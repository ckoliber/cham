import QtQuick 2.0
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.1
import Material 0.2
import Material.ListItems 0.1 as ListItem
import QtQuick.Controls.Material 2.0

Popup{
    signal sendImage(string path)
    property string imagePath:""
    property int closeState: 0
    property bool isRemovable: false
    x:parent.width/2 - width/2
    y:parent.height/2 - height/2
    contentHeight: 400
    contentWidth: 300
    id:imageEditDialogID
    onOpened: {
        if(!isRemovable){
            imagePath = "file://"+SIGNALHANDLER.copyFile(imagePath,"IMG");
            if(imagePath == ""){audioEditDialogID.close();snackbar.open("Some errors was occured !")}
        }
        closeState = 0
        movable.visible = true
        movable.width = 50
        movable.height = 50
        movable.x = 50
        movable.y = 50
    }
    onClosed: {
        if(closeState == 1){
            sendImage(imagePath)
        }else{
            SIGNALHANDLER.removeFile(imagePath,"IMG")
            // remove picture temp !
        }
    }

    View{
        backgroundColor: Palette.colors[theme.primaryColor]["200"]
        radius: 3
        elevation: 2
        width: parent.width
        height: parent.width + 50
        Rectangle{
            width: parent.width
            height: parent.width
            color:Palette.colors[theme.primaryColor]["50"]
            Image{
                cache: false
                onSourceChanged: print(sourceSize.width);
                y:parent.height/2 - height/2
                x:parent.width/2 - width/2
                id:imagebackID
                width: if(sourceSize.width >= sourceSize.height){parent.width}else{parent.width*(sourceSize.width/sourceSize.height)}
                height: if(sourceSize.width < sourceSize.height){parent.width}else{parent.width*(sourceSize.height/sourceSize.width)}
                source:imagePath

                Rectangle{
                    width: 50
                    height: 50
                    color: "#34000000"
                    x:50
                    y: 50
                    id:movable

                    Rectangle{
                        width: 10
                        height: 10
                        y:-height/2
                        radius: 10
                        color:Palette.colors[theme.primaryColor]["500"]
                        x:-width/2

                        MouseArea{
                            property int dragMouseY
                            property int dragMouseX
                            anchors.fill: parent
                            cursorShape: "SizeAllCursor"
                            onPressed:{
                                dragMouseY = mouseY
                                dragMouseX = mouseX
                            }
                            onMouseYChanged:
                                if(
                                        movable.y -(dragMouseY -mouseY) >= 0 &&
                                        movable.y -(dragMouseY -mouseY) <= imagebackID.height&&
                                        movable.height + (dragMouseY-mouseY) <= imagebackID.height &&
                                        movable.height + (dragMouseY-mouseY) >= 0
                                        ){
                                    movable.height += (dragMouseY - mouseY) ;
                                    movable.y -= (dragMouseY -mouseY) ;
                                }
                            onMouseXChanged:
                                if(
                                        movable.x -(dragMouseX -mouseX) >= 0 &&
                                        movable.x -(dragMouseX -mouseX) <= imagebackID.width&&
                                        movable.width + (dragMouseX-mouseX) <= imagebackID.width &&
                                        movable.width + (dragMouseX-mouseX) >= 0

                                        ){
                                    movable.width += (dragMouseX - mouseX) ;
                                    movable.x -= (dragMouseX -mouseX) ;
                                }

                        }


                    }

                    Rectangle{
                        width: 10
                        height: 10
                        y:-height/2
                        radius: 10
                        color:Palette.colors[theme.primaryColor]["500"]
                        x: movable.width/2 - width/2
                        MouseArea{
                            property int dragMouseY
                            anchors.fill: parent
                            cursorShape: "SizeAllCursor"
                            onPressed: dragMouseY = mouseY
                            onMouseYChanged:
                                if(
                                        movable.y - (dragMouseY -mouseY) >= 0 &&
                                        movable.y - (dragMouseY -mouseY) <= imagebackID.height &&
                                        movable.height + (dragMouseY-mouseY) <= imagebackID.height &&
                                        movable.height + (dragMouseY-mouseY) >= 0
                                        ){
                                    movable.height += (dragMouseY - mouseY) ;
                                    movable.y -= (dragMouseY -mouseY) ;
                                }
                        }

                    }

                    Rectangle{
                        width: 10
                        height: 10
                        y:-height/2
                        radius: 10
                        color:Palette.colors[theme.primaryColor]["500"]
                        x:movable.width - width/2

                        MouseArea{
                            property int dragMouseY
                            property int dragMouseX
                            anchors.fill: parent
                            cursorShape: "SizeAllCursor"
                            onPressed:{
                                dragMouseY = mouseY
                                dragMouseX = mouseX
                            }
                            onMouseYChanged:
                                if(
                                        movable.y - (dragMouseY -mouseY) >= 0 &&
                                        movable.y - (dragMouseY -mouseY) <= imagebackID.height &&
                                        movable.height + (dragMouseY-mouseY) <= imagebackID.height &&
                                        movable.height + (dragMouseY-mouseY) >= 0
                                        ){
                                    movable.height += (dragMouseY - mouseY) ;
                                    movable.y -= (dragMouseY -mouseY) ;
                                }
                            onMouseXChanged:
                                if(
                                        movable.x + movable.width - (dragMouseX-mouseX) >= 0 &&
                                        movable.x + movable.width - (dragMouseX-mouseX) <= imagebackID.width &&
                                        movable.width - (dragMouseX-mouseX) <= imagebackID.width &&
                                        movable.width - (dragMouseX-mouseX) >= 0
                                        ){
                                    movable.width -= (dragMouseX - mouseX) ;
                                }

                        }

                    }

                    Rectangle{
                        width: 10
                        height: 10
                        y:parent.height/2 - height/2
                        radius: 10
                        color:Palette.colors[theme.primaryColor]["500"]
                        x:- width/2

                        MouseArea{
                            property int dragMouseX
                            anchors.fill: parent
                            cursorShape: "SizeAllCursor"
                            onPressed: dragMouseX = mouseX
                            onMouseXChanged:
                                if(
                                        movable.x - (dragMouseX -mouseX) >= 0 &&
                                        movable.x - (dragMouseX -mouseX) <= imagebackID.width &&
                                        movable.width + (dragMouseX-mouseX) <= imagebackID.width &&
                                        movable.width + (dragMouseX-mouseX) >= 0
                                        ){
                                    movable.width += (dragMouseX - mouseX) ;
                                    movable.x -= (dragMouseX -mouseX) ;
                                }
                        }




                    }

                    Rectangle{
                        width: 10
                        height: 10
                        y:parent.height/2 - height/2
                        radius: 10
                        color:Palette.colors[theme.primaryColor]["500"]
                        x:movable.width - width/2
                        MouseArea{
                            property int dragMouseX
                            anchors.fill: parent
                            cursorShape: "SizeAllCursor"
                            onPressed: dragMouseX = mouseX
                            onMouseXChanged:
                                if(
                                        movable.x + movable.width + (mouseX - dragMouseX) >= 0 &&
                                        movable.x + movable.width + (mouseX - dragMouseX) <= imagebackID.width &&
                                        movable.width + (mouseX - dragMouseX) <= imagebackID.width &&
                                        movable.width + (mouseX - dragMouseX) >= 0
                                        ){
                                    movable.width += (mouseX - dragMouseX) ;
                                }
                        }
                    }

                    Rectangle{
                        width: 10
                        height: 10
                        y:parent.height - height/2
                        radius: 10
                        color:Palette.colors[theme.primaryColor]["500"]
                        x:- width/2

                        MouseArea{
                            property int dragMouseY
                            property int dragMouseX
                            anchors.fill: parent
                            cursorShape: "SizeAllCursor"
                            onPressed:{
                                dragMouseY = mouseY
                                dragMouseX = mouseX
                            }
                            onMouseYChanged:
                                if(
                                        movable.y + movable.height - (dragMouseY - mouseY) >= 0 &&
                                        movable.y + movable.height - (dragMouseY - mouseY) <= imagebackID.height &&
                                        movable.height - (dragMouseY - mouseY) >= 0 &&
                                        movable.height - (dragMouseY - mouseY) <= imagebackID.height
                                        ){
                                    movable.height -= (dragMouseY - mouseY) ;
                                }
                            onMouseXChanged:
                                if(
                                        movable.x - (dragMouseX -mouseX) >= 0 &&
                                        movable.x - (dragMouseX -mouseX) <= imagebackID.width &&
                                        movable.width + (dragMouseX-mouseX) <= imagebackID.width &&
                                        movable.width + (dragMouseX-mouseX) >= 0
                                        ){
                                    movable.width += (dragMouseX - mouseX) ;
                                    movable.x -= (dragMouseX -mouseX) ;
                                }

                        }

                    }

                    Rectangle{
                        width: 10
                        height: 10
                        y:parent.height - height/2
                        radius: 10
                        color:Palette.colors[theme.primaryColor]["500"]
                        x:movable.width /2 - width/2
                        MouseArea{
                            property int dragMouseY
                            anchors.fill: parent
                            cursorShape: "SizeAllCursor"
                            onPressed: dragMouseY = mouseY
                            onMouseYChanged:
                                if(
                                        movable.y + movable.height + (mouseY - dragMouseY) >= 0 &&
                                        movable.y + movable.height + (mouseY - dragMouseY) <= imagebackID.height &&
                                        movable.height + (mouseY - dragMouseY) <= imagebackID.height &&
                                        movable.height + (mouseY - dragMouseY) >= 0
                                        ){
                                    movable.height += (mouseY - dragMouseY) ;
                                }
                        }
                    }

                    Rectangle{
                        width: 10
                        height: 10
                        y:parent.height - height/2
                        radius: 10
                        color:Palette.colors[theme.primaryColor]["500"]
                        x:movable.width - width/2

                        MouseArea{
                            property int dragMouseY
                            property int dragMouseX
                            anchors.fill: parent
                            cursorShape: "SizeAllCursor"
                            onPressed:{
                                dragMouseY = mouseY
                                dragMouseX = mouseX
                            }
                            onMouseYChanged:
                                if(
                                        movable.y + movable.height - (dragMouseY - mouseY) >= 0 &&
                                        movable.y + movable.height - (dragMouseY - mouseY) <= imagebackID.height &&
                                        movable.height - (dragMouseY - mouseY) >= 0 &&
                                        movable.height - (dragMouseY - mouseY) <= imagebackID.height
                                        ){
                                    movable.height -= (dragMouseY - mouseY) ;
                                }
                            onMouseXChanged:
                                if(
                                        movable.x + movable.width + (mouseX - dragMouseX) >= 0 &&
                                        movable.x + movable.width + (mouseX - dragMouseX) <= imagebackID.width &&
                                        movable.width + (mouseX - dragMouseX) <= imagebackID.width &&
                                        movable.width + (mouseX - dragMouseX) >= 0
                                        ){
                                    movable.width += (mouseX - dragMouseX) ;
                                }

                        }

                    }

                    MouseArea{
                        property int dragMouseX
                        property int dragMouseY
                        onPressed: {
                            cursorShape = Qt.DragMoveCursor
                            dragMouseX = mouseX
                            dragMouseY = mouseY
                        }
                        onReleased: {
                            cursorShape = Qt.PointingHandCursor
                        }
                        onMouseXChanged:if(movable.x + (mouseX - dragMouseX) >= -5 && movable.x + (mouseX - dragMouseX) <= imagebackID.width-movable.width){movable.x += mouseX - dragMouseX}
                        onMouseYChanged:if(movable.y + (mouseY - dragMouseY) >= -5 && movable.y + (mouseY - dragMouseY) <= imagebackID.height-movable.height){movable.y += mouseY - dragMouseY}
                        anchors.fill: parent;
                        cursorShape: "PointingHandCursor"
                    }

                }

            }
        }

        ActionButton{
            id:imageEditDialogIDcropButtonID
            x:parent.width/2 - width/2
            y:parent.width + 10
            width: 30
            height:30
            backgroundColor: Palette.colors[theme.primaryColor]["300"]
            anchors.margins: 5
            Icon{
                id:audioEditPlayerButtonIconID
                source: "qrc:///Icons/icons/crop_30.png"
                color: "#333"
                anchors.centerIn: parent
                scale: 0.7
            }
            onClicked: {
                if(imageEditDialogIDcropButtonID.state == "EN"){
                    movable.visible = false
                    imagePath = "file://"+SIGNALHANDLER.cropImage(imagePath,movable.x*imagebackID.sourceSize.width/imagebackID.width,movable.y*imagebackID.sourceSize.width/imagebackID.width,movable.width*imagebackID.sourceSize.width/imagebackID.width,movable.height*imagebackID.sourceSize.width/imagebackID.width)
                }
            }


            state: if(movable.visible == true){"EN"}else{"DS"}
            states: [
                State {
                    name: "EN"
                    PropertyChanges {
                        target: imageEditDialogIDcropButtonID
                        backgroundColor: Palette.colors[theme.primaryColor]["300"]
                        enabled: true
                    }
                },
                State {
                    name: "DS"
                    PropertyChanges {
                        target: imageEditDialogIDcropButtonID
                        backgroundColor: "grey"
                        enabled: false
                    }
                }

            ]

        }
    }

    View{
        y:parent.width + 60
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
            onClicked:imageEditDialogID.close()
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
                imageEditDialogID.close()
            }
        }

    }

}

