import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.1
import QtQuick.Controls.Material 2.0
import QtQuick 2.0
import QtQuick.Layouts 1.0
import QtQuick.Controls 1.0
import Material 0.2
import Material.ListItems 0.1 as ListItem

ApplicationWindow {
    signal getCountries();
    signal sendCode(string code,string phone,string ccode);
    minimumWidth: 400
    visible: true
    width: 640
    height: 480
    title: qsTr("ChaM")

    CHToolBar{
        Row{
            CHIcon{
                y:2
                width:30
                height: 20
                source: "qrc:/MaterialIcons/materialicons/back.png"
            }
            Text {
                transform:Translate{x:10}
                font.pixelSize: 20
                color:"white"
                text: "ChaM"
            }
        }
    }

    Rectangle{
        z:0
        width: parent.width
        height: parent.height
        gradient: CHBrownGrad{}
    }

    Button{
        anchors.centerIn: parent
        text:"Start"
        onClicked: {
            getphonepageID.open()
        }
    }


    Snackbar {
        id: toastID
    }



    Popup{

        id:getphonepageID
        width: 400
        height: parent.height
        x: (parent.width - width) / 2
        y: (parent.height - height) / 2
        margins: 0
        padding: 0

        GetPhonePage{
            id:phonepageID
            onClosePage: getphonepageID.close()
            onGetCountry: getCountries();
            onSendCode: {
                if(phonelen != 15){
                    if( phone.length == phonelen+codelen){
                        // valid
                        startSendCode(phone);
                        parent.close()
                    }else{
                        // invalid
                        toastID.open("Enter Correct phone number !");
                    }
                }else{
                    if(phone.length >= 6){
                        // valid
                        startSendCode(phone);
                        parent.close()
                    }else{
                        // invalid
                        toastID.open("Enter Correct phone number !");
                    }
                }
            }

        }
    }


    SendCodePage{
        id:sendcodepageID
        onSendCodeToServer: sendCode(sendcodepageID.scodeID.text,getphonepageID.phoneID.text,getphonepageID.codeID.text)
    }

    function setCountry(name,code,clenghth){
        phonepageID.comboAliasID.append({"text":name});
        phonepageID.comboDataID.append({"code":code,"len":clenghth});
    }

    function setCurrentCountry(pos){
        phonepageID.comboboxID.currentIndex = pos;
    }

    function startSendCode(phonenumber){
        sendcodepageID.state = "V"
    }

    function sendCodeError(res){
        if(res == "INC"){
            toastID.open("Code is incorrect !")
        }else{
            toastID.open("Check your Network connection !")
            // Network error
        }

    }


}
