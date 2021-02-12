import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.1
import Material 0.2
import Material.ListItems 0.1 as ListItem

Dialog {

    property string targetID : ""
    property string targetType : ""
    property string targetCCode: ""
    property string targetPhone: ""

    property string name: ""
    property string uid: ""
    property string state: ""
    property string bio: ""
    property string phone: ""
    property var admins: []
    property var members: []
    property string boss: ""

    property int fontSize: 15
    width: if(parent.width <= 600){parent.width-20}else if(parent.width > 600 && parent.width <= 1000){(2*parent.width)/3}else{600}
    x: parent.width/2 - width/2
    y:parent.height/2 - height/2
    contentMargins: 0

    onOpened: {
        SOCKETHANDLER.loadTarget(targetType,targetID)
    }

    onClosed: {
        name = ""
        uid = ""
        state = ""
        bio = ""
        phone = ""
    }

    Connections{
        target: SOCKETHANDLER
        onNodeInfo:{
            if(NodeType == targetType && NodeID == targetID){
                if(Key == "NAME"){
                    name = Value
                }else if(Key == "BIO"){
                    bio = Value
                }else if(Key == "ID"){
                    uid = Value
                }else if(Key == "PIC"){

                }else if(Key == "ADMINS"){

                }else if(Key == "MEMBERS"){

                }else if(Key == "BOSS"){

                }
            }
        }
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
                source: "qrc:///Icons/icons/message-text.png"
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
            Label{
                font.pixelSize: 20
                y:parent.height/3 - height/3
                x: dp(120)
                text:name
            }
            Label{
                font.pixelSize: 13
                y:parent.height*2/3 - height*2/3
                x: dp(120)
                text:state
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
        height: 40
        elevation: 1
        Label{
            y:parent.height/2 - height/2
            x: 5
            font.pixelSize: fontSize
            text:"ID : "
            font.weight: Font.bold
        }

        Label {
            y:parent.height/2 - height/2
            x: parent.width - width - 10
            width: parent.width - 100
            wrapMode: "Wrap"
            text:uid
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
            text:"Bio : "
            font.weight: Font.bold
        }

        Label{
            y:parent.height/2 - height/2
            x: parent.width - width - 10
            width: parent.width - 100
            wrapMode: "Wrap"
            text:bio
        }

    }

    View{
        visible: targetPhone != ""
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
            font.weight: Font.bold
        }

        Label {
            y:parent.height/2 - height/2
            x: parent.width - width - 10
            text:"+"+targetCCode + "-"+targetPhone
        }

    }

}
