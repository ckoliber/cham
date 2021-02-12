import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.1
import Material 0.2
import Material.ListItems 0.1 as ListItem

Dialog {
    property int fontSize: 15
    width: if(parent.width <= 600){parent.width-20}else if(parent.width > 600 && parent.width <= 1000){(2*parent.width)/3}else{600}
    x: parent.width/2 - width/2
    y:parent.height/2 - height/2
    contentMargins: 0

    BottomActionSheet {
        id: actionSheet
        title: "From Local"
        actions: [
            Action {
                iconSource: "qrc:///Icons/icons/camera-iris.png"
                name: "From Camera"
            },
            Action {
                iconSource: "qrc:///Icons/icons/folder.png"
                name: "From Storage"
            }
        ]
    }

    View{
        width: parent.width
        height: dp(125)
        ActionButton {
            isMiniSize: true
            z:100
            x:parent.width - width - 20
            y:dp(100) - height/2
            backgroundColor: Palette.colors[theme.primaryColor]["300"]
            Icon{
                anchors.centerIn: parent
                color: "#333"
                source: "qrc:///Icons/icons/camera-iris.png"
            }
            onClicked:actionSheet.open()
        }
        View{
            width: parent.width
            height: dp(100)
            backgroundColor: Palette.colors[theme.primaryColor]["500"]

            Image {
                source: "qrc:///Data/data/icon.png"
                width:dp(80)
                height: dp(80)
                y:parent.height/2 - height/2
                x: 20
            }
            TextField {
                y:parent.height/2 - height/2
                x: (parent.width - dp(80))/2 - width/2 + dp(80)
                color: "white"
                placeholderText: "Name"
                floatingLabel: true
                hasError: false
                helperText: "Hint : Enter your Name !"
            }

        }

        View{
            y:100
            width: parent.width
            height: 30

        }

    }

    View{
        backgroundColor: "#ddd"
        width: parent.width - 30
        x:parent.width/2 - width/2
        height: 60
        elevation: 1
        Label{
            y:parent.height/2 - height/2
            x: 5
            font.pixelSize: fontSize
            text:"ID : "
        }

        TextField {
            y:parent.height/2 - height/2
            x: parent.width - width - 10
            placeholderText: "ID"
            floatingLabel: true
            hasError: false
        }

    }

    View{
        backgroundColor: "#ddd"
        width: parent.width - 30
        x:parent.width/2 - width/2
        height: 40
        elevation: 1
        Label{
            y:parent.height/2 - height/2
            x: 5
            font.pixelSize: fontSize
            text:"Phone : "
        }

        Label {
            y:parent.height/2 - height/2
            x: parent.width - width - 10
            text:"+98-9123456789"
        }

    }

    View{
        backgroundColor: "#ddd"
        width: parent.width - 30
        x:parent.width/2 - width/2
        height: 40
        elevation: 1
        Label{
            y:parent.height/2 - height/2
            x: 5
            font.pixelSize: fontSize
            text:"Last seen : "
        }

        MenuField {
            y:parent.height/2 - height/2
            x: parent.width - width - 10
            model: ["Show to all","Just show to my contacts","Hide from all"]
        }
    }

    View{
        backgroundColor: "#ddd"
        width: parent.width - 30
        x:parent.width/2 - width/2
        height: 40
        elevation: 1
        Label{
            y:parent.height/2 - height/2
            x: 5
            font.pixelSize: fontSize
            text:"Blocks : "
        }

        Button {
            y:parent.height/2 - height/2
            x: parent.width - width - 10
            text:"Show"
        }

    }

}