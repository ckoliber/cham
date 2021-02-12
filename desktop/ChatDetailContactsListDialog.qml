import QtQuick 2.7
import QtQuick.Controls 2.0
import Material 0.2
import QtMultimedia 5.0
import Material.ListItems 0.1 as ListItem
import QtQuick.Layouts 1.3
import QtQuick.Dialogs 1.2 as Dialogs

Dialog{
    signal sendContact(string name,string phone);
    property string selectedName: ""
    property string selectedPhone: ""
    id:contactsListDialogID
    contentMargins: 0
    hasActions: false
    onOpened: {
        selectedName = ""
        selectedPhone = ""
        contactsModelID.clear()
        // init list model
        for(var a = 0 ; a < 10 ; a++){
            contactsModelID.append({"CNA":"ali"+a,"CPH":a+""});
        }
    }


    function showContact(index){
        selectedName = contactsModelID.get(index).CNA
        selectedPhone = contactsModelID.get(index).CPH
        contactShowDialogID.open()
    }


    ListModel{id:contactsModelID}
    View{width: parent.width; height: 10}
    Repeater{
        width: parent.width
        model:contactsModelID
        delegate: View{

            Ink{
                anchors.fill: parent
                onClicked:{showContact(index);contactsListDialogID.close()}
            }

            width: parent.width - 20
            x:10
            y: 5
            height: 50
            backgroundColor: Palette.colors[theme.primaryColor]["200"]
            radius: 3
            View{
                x:7.5
                y:7.5
                width: 35
                height: 35
                radius: 35
                backgroundColor: "white"
                Image{
                    anchors.centerIn: parent
                    source: "qrc:///Icons/icons/account.png"
                    sourceSize.width: 30
                    sourceSize.height: 30
                }
            }
            Text{
                y:parent.height/3 - height/2
                x:50
                width: parent.width - 10
                elide: Text.ElideRight
                text:"<b>Name :</b> "+contactsModelID.get(index).CNA
            }

            Text{
                y:parent.height*2/3 - height/2
                x:50
                width: parent.width - 10
                elide: Text.ElideRight
                text:"<b>Phone :</b> "+contactsModelID.get(index).CPH


            }

        }
    }
    View{width: parent.width; height: 10}



    Dialog{
        id:contactShowDialogID
        contentMargins: 0
        hasActions: false

        View{
            width: parent.width
            height: 60

            View{
                width: 50
                height: 50
                x:5
                y:5
                elevation: 2
                radius: 50
                backgroundColor: Palette.colors[theme.primaryColor]["400"]

                Icon {
                    anchors.centerIn: parent
                    id: fileIcon
                    source: "qrc:///Icons/icons/contact.png"
                }
            }

            View{
                radius: 3
                elevation: 1
                width: parent.width - 70
                y:5
                x:60
                height: 50
                backgroundColor: Palette.colors[theme.primaryColor]["200"]
                Text{
                    width: parent.width - 5
                    elide: Text.ElideRight
                    x:5
                    y:parent.height/3 - height/2
                    text:"<b>Name :</b> " + selectedName
                }

                Text{
                    width: parent.width - 5
                    elide: Text.ElideRight
                    x:5
                    y:parent.height*2/3 - height/2
                    text:"<b>Phone :</b> " + selectedPhone
                }
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
                onClicked:contactShowDialogID.close()
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
                onClicked:{sendContact(selectedName,selectedPhone) ; contactShowDialogID.close()}
            }

        }

    }


}
