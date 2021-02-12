import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.1
import QtQuick 2.0
import QtQuick.Layouts 1.0
import QtQuick.Controls 1.0
import Material 0.2
import Material.ListItems 0.1 as ListItem

Pane{

    Component.onCompleted: {
        SIGNALHANDLER.loadMainList();
    }

    Connections{
        target: SIGNALHANDLER
        onMessage:onListItem(DataType,
                             MDate,
                             From,
                             FromGroupOrChannelMemberID,
                             FromType,
                             TextData,
                             EmojiCode,
                             ContactName,
                             ContactPhone,
                             LocationAlt,
                             LocationLat,
                             LocationLong,
                             StreamLink,
                             BufferSource,
                             StreamType,
                             isSelf,
                             MessagesCount)
    }

    Connections{
        target: SOCKETHANDLER
        onMessage:onListItem(DataType,
                             MDate,
                             From,
                             FromGroupOrChannelMemberID,
                             FromType,
                             TextData,
                             EmojiCode,
                             ContactName,
                             ContactPhone,
                             LocationAlt,
                             LocationLat,
                             LocationLong,
                             StreamLink,
                             BufferSource,
                             StreamType,
                             "F",
                             1)
    }

    ListModel{id:messengerListModelID}


    function onListItem(DataType,
                        MDate,
                        From,
                        FromGroupOrChannelMemberID,
                        FromType,
                        TextData,
                        EmojiCode,
                        ContactName,
                        ContactPhone,
                        LocationAlt,
                        LocationLat,
                        LocationLong,
                        StreamLink,
                        BufferSource,
                        StreamType,
                        isSelf,
                        MessagesCount){
        var Data;
        switch(DataType){
        case "TXT":
            Data = FromGroupOrChannelMemberID +" "+ TextData;
            break;
        case "EMJ":
            Data = FromGroupOrChannelMemberID +" "+ "Emoji";
            break;
        case "CNT":
            Data = FromGroupOrChannelMemberID +" "+ ContactName;
            break;
        case "LOC":
            Data = FromGroupOrChannelMemberID +" "+ "Location";
            break;
        case "DST":
            Data = FromGroupOrChannelMemberID +" "+ "Data";
            break;
        }
        var find = 0;
        for(var a = 0 ; a < messengerListModelID.count ; a++){
            if(messengerListModelID.get(a).FID == From && messengerListModelID.get(a).FRT == FromType){
                find = 1;
                messengerListModelID.setProperty(a,"MCOUNT",parseInt(messengerListModelID.get(a).MCOUNT) + 1+"");
                messengerListModelID.setProperty(a,"DATA",Data);
                messengerListModelID.setProperty(a,"DATE",MDate);
                messengerListModelID.move(a,0,1);
                break;
            }
        }

        if(find == 0){
            messengerListModelID.append({"FID":From,"FRT":FromType,"MCOUNT":MessagesCount+"","DATA":Data,"DATE":MDate,"PIC":"PictureAddress"});
        }


    }

    width: if(parent.width > 600){parent.width / 3}else{parent.width}
    height: parent.height;

    ListView{
        ScrollBar.vertical: ScrollBar {}
        id:listItemsID
        property int beforeSelection: -1;
        function selectedItemChanged(index){
            if(beforeSelection != index){
                SIGNALHANDLER.loadItemDetails(messengerListModelID.get(index).FID,messengerListModelID.get(index).FRT)
                listItemsID.contentItem.children[index].paneID.state = "ORANGE"
                if(beforeSelection != -1){
                    listItemsID.contentItem.children[beforeSelection].paneID.state = "BLUE"
                    listItemsID.contentItem.children[beforeSelection].visSelected = false
                }
            }
            beforeSelection = index;
        }
        anchors.fill: parent
        model:messengerListModelID;
        delegate: listDelegate
        spacing: 10
    }

    Component{
        id:listDelegate
        Row{
            property alias paneID: paneID
            property bool visSelected: false
            width: parent.width
            height: 50
            View{
                radius: 4
                states:[
                    State {
                        name: "BLUE"
                        PropertyChanges {
                            target: paneID
                            backgroundColor: "#333"
                        }
                    },
                    State {
                        name: "ORANGE"
                        PropertyChanges {
                            target: paneID
                            backgroundColor: Palette.colors["orange"]["500"]
                        }
                    },
                    State {
                        name: "RED"
                        PropertyChanges {
                            target: paneID
                            backgroundColor: Palette.colors["red"]["500"]
                        }
                    }
                ]
                state:"BLUE"

                MouseArea{
                    cursorShape: "PointingHandCursor"
                    anchors.fill: parent
                    hoverEnabled: true
                    onHoveredChanged: if(!visSelected){if(containsMouse){paneID.state = "RED"}else{paneID.state = "BLUE"}}
                    onClicked: {visSelected = true;parent.parent.parent.parent.parent.selectedItemChanged(index)}
                }
                id:paneID
                elevation:4
                backgroundColor: Palette.colors["grey"]["500"]
                width: parent.width
                height:50
                Image{
                    y:1
                    x:1
                    source: "qrc:///Data/data/icon.png"
                    width: 50
                    height: 50
                }
                Text{
                    elide: Text.ElideRight
                    width: parent.width - 120
                    color:"white"
                    x:50
                    y:10
                    text:model.FID;
                }
                Text{
                    elide: Text.ElideRight
                    width: 60
                    color:"white"
                    x:parent.width - width - 5
                    y:10
                    text:model.DATE;
                }
                Text{
                    elide: Text.ElideRight
                    width: parent.width - 50 - (model.MCOUNT.length)*10 - 10
                    color:"white"
                    x:50
                    y:30
                    text:model.DATA;
                }
                View{
                    radius: 2
                    height: 14
                    width: (model.MCOUNT.length)*10
                    y:30
                    anchors.margins: 2
                    x:parent.width - width - 5
                    backgroundColor: Palette.colors["blue"]["500"]

                    Text{
                        x:parent.width/2 - width/2
                        color:"black"
                        text:model.MCOUNT;
                    }

                }

            }
        }



    }


}
