import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.1
import Material 0.2

Column{
    signal closePage();
    signal getCountry();
    signal sendCode(string phone,int codelen,int phonelen);
    property alias comboAliasID: comboModelID
    property alias comboDataID: comboDataID
    property alias comboboxID: comboboxID
    property alias phoneID: phoneID
    property alias codeID: codeID

    id:getphonepageID
    z:25
    anchors.fill: parent


    Timer{
        running: true
        interval: 0;
        repeat: false;
        onTriggered: getCountry();
    }


    ListModel{id:comboModelID}
    ListModel{id:comboDataID}

    Card{
        anchors.fill: parent
        id:cardID
        state:"V"
        onClosed:closePage();
        Column{
            spacing: 5
            anchors.centerIn: parent
            Text{
                text:"Enter your phone number :"
            }
            MenuField{
                id:comboboxID
                Material.accent: Material.Brown
                model:comboModelID
                height: 40
                width: 300
                y:5
                currentIndex: 0;
                onCurrentIndexChanged:{
                    codeID.text = "+"+comboDataID.get(currentIndex).code;
                    if(comboDataID.get(currentIndex).len != "-1"){
                        phoneID.maximumLength = comboDataID.get(currentIndex).len.replace(" ","").replace(" ","").replace(" ","").replace(" ","").length-1;
                    }else{
                        phoneID.maximumLength = 15;
                    }
                }
            }
            Row{
                spacing: 5
                Text{
                    id:codeID
                    font.pixelSize: 20
                    text:"+1876"
                    height: 30
                    width: 100
                    y:10
                    transform: Translate{x:20}
                }
                TextField{
                    id:phoneID
                    Material.accent: Material.Brown
                    width:230
                    validator: RegExpValidator{regExp: /[0-9]+/}
                    maximumLength: 7
                    placeholderText: "Enter phone number..."
                }
            }
            Button{
                x: 120
                text:"Send Code"
                onClicked: sendCode(codeID.text+phoneID.text,codeID.text.length,phoneID.maximumLength);
            }

        }
    }
}


