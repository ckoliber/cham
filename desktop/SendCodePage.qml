import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.1
import Material 0.2

Rectangle{
    signal sendCodeToServer(string code);
    id:scodepageID
    state:"H"
    z:25
    width: parent.width
    height: parent.height
    color:"#55555555"
    property alias scodeID: scodeID
    property int minute: 3
    property int second: 0

    Card{
        height:parent.height
        width:380
        id:cardID
        state:"V"
        onClosed:parent.state = "H";
        Column{
            spacing: 5
            anchors.centerIn: parent
            Text{
                text:"Enter Code :"
            }

            TextField{
                id:scodeID
                Material.accent: Material.Brown
                width:230
                validator: RegExpValidator{regExp: /[0-9]+/}
                maximumLength: 6
                placeholderText: "Enter code..."
            }

            Button{
                x: 120
                text:"Send Code"
                onClicked:sendCodeToServer(scodeID)
            }
        }
    }

    onStateChanged: if(state == "V"){cardID.state = "V"}else{cardID.state = "H"}
    states: [
        State {
            name: "V"
            PropertyChanges {
                target: scodepageID
                visible: true
                opacity: 1
            }
        },
        State {
            name: "H"
            PropertyChanges {
                target: scodepageID
                visible: false
                opacity: 0
            }
        }
    ]
    transitions: [
            Transition {
                from: "V"
                to: "H"
                PropertyAnimation{
                    target: scodepageID
                    property: "opacity"
                    duration: 300
                    to : 0
                }
            },
            Transition {
                from: "H"
                to: "V"
                PropertyAnimation{
                    target: scodepageID
                    property: "opacity"
                    duration: 300
                    to : 1
                }
            }
        ]

}


