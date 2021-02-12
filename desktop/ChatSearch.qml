import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.1
import Material 0.2
import Material.ListItems 0.1 as ListItem
import QtQuick.Controls 1.3 as QuickControls

Dialog {
    width : 250
    height: 220
    focus: true
    id:searchDialog
    contentMargins: 0
    hasActions: false
    property int currentSelection: 0
    onOpened: {
        targetID.text = ""
        currentSelection = 0
        radioOne.checked = true
        radioTwo.checked = false
        radioThree.checked = false
    }

    QuickControls.ExclusiveGroup {id: optionGroup}

    Connections{
        target: SOCKETHANDLER
        onSearchResult:{
            progressDialog.close()
            if(found == "OK"){
                searchDialog.close()
                if(currentSelection == 0){
                    openUser(targetID.text,"CLT")
                }else if(currentSelection == 1){
                    openUser(targetID.text,"GPT")
                }else{
                    openUser(targetID.text,"CHT")
                }
            }else{
                if(currentSelection == 0){
                    snackbar.open("Could not find client !")
                }else if(currentSelection == 1){
                    snackbar.open("Could not find group !")
                }else{
                    snackbar.open("Could not find channel !")
                }
            }
        }
    }

    function openUser(userID,userType){
        account.targetType =  userType
        account.targetID = userID
        account.open()
    }

    View{
        width: searchDialog.width
        height: searchDialog.height-5

        TextField {
            id:targetID
            y:20
            x: parent.width/2 - width/2
            placeholderText: "ID..."
            floatingLabel: true
            characterLimit: 20
            validator: RegExpValidator{regExp: /[0-9a-z-A-Z]+/ }
        }

        RadioButton {
            id:radioOne
            y:60
            x:10
            checked: true
            text: "Client ID"
            canToggle: false
            exclusiveGroup: optionGroup
            onClicked: currentSelection = 0
        }

        RadioButton {
            id:radioTwo
            y:90
            x:10
            text: "Group ID"
            canToggle: false
            exclusiveGroup: optionGroup
            onClicked: currentSelection = 1
        }

        RadioButton {
            id:radioThree
            y:120
            x:10
            text: "Channel ID"
            canToggle: false
            exclusiveGroup: optionGroup
            onClicked: currentSelection = 2
        }

        Row{
            y:parent.height - height - 5
            x: parent.width /2- width/2
            Button{
                text:"Cancel"
                onClicked:searchDialog.close()
            }

            Button{
                text: "Search"
                onClicked:{
                    progressDialog.open()
                    if(currentSelection == 0){
                        SOCKETHANDLER.searchTarget("CLT",targetID.text)
                    }else if(currentSelection == 1){
                        SOCKETHANDLER.searchTarget("GPT",targetID.text)
                    }else{
                        SOCKETHANDLER.searchTarget("CHT",targetID.text)
                    }
                }
            }

        }

    }

    Dialog{
        id:progressDialog
        height: 200
        View{
            anchors.fill: parent
            ProgressCircle {
                Layout.alignment: Qt.AlignCenter
                color: Palette.colors[theme.primaryColor]["500"]
            }
            Text{
                text:"Please wait !"
            }
        }
    }
}
