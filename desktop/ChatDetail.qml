import QtQuick 2.7
import QtQuick.Controls 2.0
import Material 0.2
import QtMultimedia 5.0
import Material.ListItems 0.1 as ListItem
import QtQuick.Layouts 1.3
import QtQuick.Dialogs 1.2 as Dialogs
import QtQuick.Controls.Material 2.0

View{

    Image {
        anchors.fill: parent
        source: "qrc:///Data/data/balloon.jpg"
    }

    property string targetID: ""
    property string targetType: ""
    property string selfID: ""
    property string selfType: ""

//    Component.onCompleted: {
//        for(var b = 0 ; b < 120 ; b ++ ){
//            messengerDetailModelID.append({"DT":"TXT","FROM":"ali","FRID":"","MDATE":"2016","FRT":"CLT","ISSELF":b%3 == 0,"TXT":"Helsadddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddlo World","EMJ":"","CNA":"","CPH":"","ALT":"","LAT":"","LONG":"","LNK":"","ST":"","SRC":"","STATE":"TXT"});
////            messengerDetailModelID.append({"DT":"EMJ","FROM":"ali","FRID":"","DATE":"2016","FRT":"CLT","ISSELF":b%3 == 0,"TXT":"","EMJ":"World","CNA":"","CPH":"","ALT":"","LAT":"","LONG":"","LNK":"","ST":"","SRC":"","STATE":"EMJ"});
////            messengerDetailModelID.append({"DT":"CNT","FROM":"ali","FRID":"","DATE":"2016","FRT":"CLT","ISSELF":b%3 == 0,"TXT":"","EMJ":"","CNA":"ali","CPH":"+989380851109","ALT":"","LAT":"","LONG":"","LNK":"","ST":"","SRC":"","STATE":"CNT"});
////            messengerDetailModelID.append({"DT":"LOC","FROM":"ali","FRID":"","DATE":"2016","FRT":"CLT","ISSELF":b%3 == 0,"TXT":"","EMJ":"","CNA":"","CPH":"","ALT":"1","LAT":"1","LONG":"1","LNK":"","ST":"","SRC":"","STATE":"LOC"});
////            messengerDetailModelID.append({"DT":"DST","FROM":"ali","FRID":"","DATE":"2016","FRT":"CLT","ISSELF":b%3 == 0,"TXT":"","EMJ":"","CNA":"","CPH":"","ALT":"","LAT":"","LONG":"","LNK":"112A1241","ST":"IMG","SRC":"","STATE":"IMG"});
////            messengerDetailModelID.append({"DT":"SHN","FROM":"ali","FRID":"","DATE":"2016","FRT":"CLT","ISSELF":b%3 == 0,"TXT":"Ali Created a Video Session !","EMJ":"","CNA":"","CPH":"","ALT":"","LAT":"","LONG":"","LNK":"","ST":"","SRC":"","STATE":"SHN"});
////            messengerDetailModelID.append({"DT":"INF","FROM":"ali","FRID":"","DATE":"2016","FRT":"CLT","ISSELF":b%3 == 0,"TXT":"Reza Left the Group !","EMJ":"","CNA":"","CPH":"","ALT":"","LAT":"","LONG":"","LNK":"","ST":"","SRC":"","STATE":"INF"});
//        }
//    }

    Connections{
        target: SIGNALHANDLER
        onClearDetail:{sendBox.visible = true;messengerDetailModelID.clear()}
        onSetTargetPage:{targetID = TargetID;targetType = TargetType}
        onMessageDetail:onMessage( DataType,
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
                                   isSelf);
    }

    Connections{
        target: SOCKETHANDLER
        onMessage:if(From == targetID && FromType == targetType){onMessage(DataType,
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
                                                                            false)}
    }

    function onMessage(DataType,
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
                       isSelf){
        var MState = "";
        if(DataType != "DST"){
            MState = DataType;
        }else{
            MState = StreamType;
        }
        messengerDetailModelID.append({"DT":DataType,"FROM":From,"FRID":FromGroupOrChannelMemberID,"MDATE":MDate,"FRT":FromType,"ISSELF":isSelf,"TXT":TextData,"EMJ":EmojiCode,"CNA":ContactName,"CPH":ContactPhone,"ALT":LocationAlt,"LAT":LocationLat,"LONG":LocationLong,"LNK":StreamLink,"ST":StreamType,"SRC":BufferSource,"STATE":MState});
    }

    function sendMessageText(DataType,
                             MDate,
                             TargetType,
                             TargetID,
                             FromID,
                             TextData,
                             EmojiCode,
                             ContactName,
                             ContactPhone,
                             LocationAlt,
                             LocationLat,
                             LocationLong,
                             BufferType,
                             BufferSize){
        SOCKETHANDLER.sendMessage();
        messengerDetailModelID.append({"DT":DataType,"FROM":FromID,"FRID":FromID,"MDATE":DATEHANDLER.getDate(),"FRT":"CLT","ISSELF":true,"TXT":TextData,"EMJ":EmojiCode,"CNA":ContactName,"CPH":ContactPhone,"ALT":LocationAlt,"LAT":LocationLat,"LONG":LocationLong,"LNK":"","ST":BufferType,"SRC":BufferSize,"STATE":BufferType});
    }

    ListModel{id:messengerDetailModelID}
    backgroundColor: "white";
    width: parent.width*2 / 3;
    height: parent.height;

    ListView{
        anchors.margins: 10
        ScrollBar.vertical: ScrollBar {}
        width: parent.width - 10
        x: parent.width/2 - width/2
        height: parent.height - 65
        model: messengerDetailModelID
        delegate: listDelegate
        spacing: 10
    }

    View{
        id:sendBox
        visible: false
        x:parent.width/2 - width/2
        y:parent.height - height - 10
        width: parent.width
        height: textBoxViewID.height+10

        ActionButton{
            width: 40
            height: 40
            y:parent.height - height - 5
            x:10
            backgroundColor: Palette.colors[theme.primaryColor]["300"]
            Icon{
                id:emojiIcon
                source: "qrc:///Icons/icons/emoji.png"
                color: "#333"
                anchors.centerIn: parent
            }
            onClicked: emojiDialogID.open();
        }

        View{
            id:textBoxViewID
            width: parent.width - 170
            height: if(40+(textID.lineCount-1)*20 > 250){250}else{40+(textID.lineCount-1)*20}
            y:parent.height/2 - height/2
            x:60
            elevation: 3
            backgroundColor: Palette.colors[theme.primaryColor]["200"]
            radius: 3

            Flickable {
                flickableDirection: Flickable.VerticalFlick
                anchors.leftMargin: 5
                anchors.rightMargin: 5
                id:textFlickableID
                anchors.fill: parent
                TextArea.flickable: TextArea {
                    placeholderText: "Enter Text..."
                    wrapMode: "Wrap"
                    color: "black"
                    Material.accent: Palette.colors[theme.primaryColor]["500"]
                    textFormat: TextEdit.RichText
                    id:textID
                    onTextChanged: {if(textID.text != ""){micIcon.source = "qrc:///Icons/icons/send.png"}else{micIcon.source = "qrc:///Icons/icons/microphone.png"}}
                }

                ScrollBar.vertical: ScrollBar { }
            }

        }

        ActionButton{
            width: 40
            height: 40
            y:parent.height - height - 5
            x:parent.width - width - 60
            backgroundColor: Palette.colors[theme.primaryColor]["300"]
            Icon{
                id:attachIcon
                source: "qrc:///Icons/icons/attachment.png"
                color: "#333"
                anchors.centerIn: parent
            }
            onClicked: actionSheet.open()
        }
        ActionButton{
            width: 40
            height: 40
            y:parent.height - height - 5
            x:parent.width - width - 10
            backgroundColor: Palette.colors[theme.primaryColor]["300"]

            Icon{
                id:micIcon
                source: "qrc:///Icons/icons/microphone.png"
                color: "#333"
                anchors.centerIn: parent
            }
            onClicked: if(micIcon.source == "qrc:///Icons/icons/microphone.png"){audioRecordDialogID.open()}else{sendMessageText("TXT",DATEHANDLER.getDate(),targetType,targetID,selfID,textID.text,null,null,null,null,null,null,null,null)}
        }
    }

    Component{
        id:listDelegate
        Row{
            states:[
                State {
                    name: "TXT"
                    PropertyChanges {
                        target: textMessageID
                        visible:true
                    }
                    PropertyChanges {
                        target: emojiMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: locationMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: contactMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: imageMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: audioMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: videoMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: fileMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: sessionMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: infoMessageID
                        visible:false
                    }
                },
                State {
                    name: "EMJ"
                    PropertyChanges {
                        target: textMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: emojiMessageID
                        visible:true
                    }
                    PropertyChanges {
                        target: locationMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: contactMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: imageMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: audioMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: videoMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: fileMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: sessionMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: infoMessageID
                        visible:false
                    }
                },
                State {
                    name: "LOC"
                    PropertyChanges {
                        target: textMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: emojiMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: locationMessageID
                        visible:true
                    }
                    PropertyChanges {
                        target: contactMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: imageMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: audioMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: videoMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: fileMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: sessionMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: infoMessageID
                        visible:false
                    }
                },

                State {
                    name: "CNT"
                    PropertyChanges {
                        target: textMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: emojiMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: locationMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: contactMessageID
                        visible:true
                    }
                    PropertyChanges {
                        target: imageMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: audioMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: videoMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: fileMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: sessionMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: infoMessageID
                        visible:false
                    }
                },
                State {
                    name: "IMG"
                    PropertyChanges {
                        target: textMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: emojiMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: locationMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: contactMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: imageMessageID
                        visible:true
                    }
                    PropertyChanges {
                        target: audioMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: videoMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: fileMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: sessionMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: infoMessageID
                        visible:false
                    }
                },
                State {
                    name: "AUD"
                    PropertyChanges {
                        target: textMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: emojiMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: locationMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: contactMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: imageMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: audioMessageID
                        visible:true
                    }
                    PropertyChanges {
                        target: videoMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: fileMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: sessionMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: infoMessageID
                        visible:false
                    }
                },
                State {
                    name: "VDO"
                    PropertyChanges {
                        target: textMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: emojiMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: locationMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: contactMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: imageMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: audioMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: videoMessageID
                        visible:true
                    }
                    PropertyChanges {
                        target: fileMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: sessionMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: infoMessageID
                        visible:false
                    }
                },
                State {
                    name: "FIL"
                    PropertyChanges {
                        target: textMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: emojiMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: locationMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: contactMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: imageMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: audioMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: videoMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: fileMessageID
                        visible:true
                    }
                    PropertyChanges {
                        target: sessionMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: infoMessageID
                        visible:false
                    }
                },
                State {
                    name: "SHN"
                    PropertyChanges {
                        target: textMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: emojiMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: locationMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: contactMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: imageMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: audioMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: videoMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: fileMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: sessionMessageID
                        visible:true
                    }
                    PropertyChanges {
                        target: infoMessageID
                        visible:false
                    }
                },
                State {
                    name: "INF"
                    PropertyChanges {
                        target: textMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: emojiMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: locationMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: contactMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: imageMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: audioMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: videoMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: fileMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: sessionMessageID
                        visible:false
                    }
                    PropertyChanges {
                        target: infoMessageID
                        visible:true
                    }
                }
            ]

            spacing:5
            state:model.STATE
            x:if(parent.width > 500){if(model.ISSELF){parent.width /3}else{0}}else{if(model.ISSELF){50}else{0}}
            width: if(parent.width > 500){parent.width*2 / 3}else{parent.width-50}

            View{
                id:textMessageID
                radius: 5
                elevation : 1
                backgroundColor: if(model.ISSELF){Palette.colors[theme.primaryColor]["200"]}else{Palette.colors["grey"]["400"]}
                width: parent.width
                height: 50 + textID.height
                Column{
                    anchors.fill: parent
                    anchors.margins: 10
                    Text{
                        x:parent.width - width
                        wrapMode: Text.Wrap
                        text:model.MDATE;
                    }

                    Text{
                        transform: Translate{y:10}
                        id:textID
                        width: parent.width
                        wrapMode: Text.Wrap
                        text:model.TXT;
                    }

                }

            }

            View{
                id:emojiMessageID
                radius: 5
                backgroundColor: "#00000000"
                width: parent.width
                height: 50 + emojiID.height
                Column{
                    anchors.fill: parent
                    anchors.margins: 10
                    Text{
                        x:parent.width - width
                        wrapMode: Text.Wrap
                        text:model.MDATE;
                    }
                    Image{
                        id:emojiID
                        width: parent.width
                        height: (parent.width/sourceSize.width)*sourceSize.height
                        //source: model.EMJ
                    }
                }
            }

            View{
                id:locationMessageID
                radius: 5
                elevation : 2
                backgroundColor: if(model.ISSELF){Palette.colors[theme.primaryColor]["300"]}else{Palette.colors["grey"]["400"]}
                width: if(parent.width > 300){300}else{parent.width}
                x: if(parent.width > 300){parent.width - 300}else{0}

                Column{
                    anchors.fill: parent
                    anchors.margins: 10

                    Row{

                        Image{
                            source: "qrc:///Icons/icons/maps_place.svg"
                            width: 40
                            height: 40
                        }

                        Text{
                            x:parent.width - width
                            wrapMode: Text.Wrap
                            text:"Location";
                        }
                    }
                }
            }

            View{
                id:contactMessageID
                radius: 5
                elevation : 2
                backgroundColor: if(model.ISSELF){Palette.colors[theme.primaryColor]["300"]}else{Palette.colors["grey"]["400"]}
                width: parent.width
                height: 50 + contactID.height
                Column{
                    anchors.fill: parent
                    anchors.margins: 10

                    Row{

                        Image{
                            source: "qrc:///Icons/icons/account.png"
                            width: 40
                            height: 40
                        }

                        Column{
                            Text{
                                x:parent.width - width
                                wrapMode: Text.Wrap
                                text:model.CNA;
                            }
                            Text{
                                id:contactID
                                width: parent.width
                                text: model.CPH
                            }
                        }
                    }
                }
            }

            View{
                id:imageMessageID
                radius: 5
                elevation : 2
                backgroundColor: if(model.ISSELF){Palette.colors[theme.primaryColor]["300"]}else{Palette.colors["grey"]["400"]}
                width: parent.width
                height: 180
                Image {
                    visible: false
                    Ink {
                        anchors.fill: parent
                        onClicked: overlayView.open(parent)
                    }
                    width: parent.width
                    height: parent.width
                    source: "qrc:///Data/data/icon.png"
                }
            }

            View{
                id:audioMessageID
                radius: 5
                elevation : 2
                backgroundColor: if(model.ISSELF){Palette.colors[theme.primaryColor]["300"]}else{Palette.colors["grey"]["400"]}
                width: parent.width
                height: 180


            }

            View{
                id:videoMessageID
                radius: 5
                elevation : 2
                backgroundColor: if(model.ISSELF){Palette.colors[theme.primaryColor]["300"]}else{Palette.colors["grey"]["400"]}
                width: parent.width
                height: 180

            }

            View{
                id:fileMessageID
                radius: 5
                elevation : 2
                backgroundColor: if(model.ISSELF){Palette.colors[theme.primaryColor]["300"]}else{Palette.colors["grey"]["400"]}
                width: parent.width
                height: 180

            }

            View{
                id:sessionMessageID
                radius: 5
                elevation : 2
                backgroundColor: if(model.ISSELF){Palette.colors[theme.primaryColor]["300"]}else{Palette.colors["grey"]["400"]}
                width: parent.width
                height: 180

            }

            View{
                id:infoMessageID
                radius: 5
                elevation : 2
                backgroundColor: if(model.ISSELF){Palette.colors[theme.primaryColor]["300"]}else{Palette.colors["grey"]["400"]}
                width: parent.width
                height: 180

            }

        }


    }

    BottomActionSheet {
        id: actionSheet
        title: "Attachment"
        actions: [
            Action {
                iconSource: "qrc:///Icons/icons/camera-iris.png"
                name: "Take picture"
                onTriggered: {actionSheet.close();imageCaptureDialogID.open();}
            },
            Action {
                iconSource: "qrc:///Icons/icons/video.png"
                name: "Record video"
                onTriggered: {actionSheet.close();videoRecordDialogID.open()}
            },
            Action {
                iconSource: "qrc:///Icons/icons/folder.png"
                name: "File From Storage"
                onTriggered: {actionSheet.close();fileSelectionDialogID.open()}
            },
            Action {
                iconSource: "qrc:///Icons/icons/contact.png"
                name: "Contact"
                onTriggered: {actionSheet.close();contactsListDialogID.open()}
            }
        ]
    }

    Dialogs.FileDialog {
        id: fileSelectionDialogID
        title: "Please choose a file"
        folder: shortcuts.home
        selectFolder: false
        selectMultiple: false
        onAccepted: {
            switch(SIGNALHANDLER.getFileFormat(fileSelectionDialogID.fileUrl)){
            case 0:
                audioEditDialogID.audioPath = fileSelectionDialogID.fileUrl;
                audioEditDialogID.isRemovable = false
                audioEditDialogID.open()
                break;
            case 1:
                imageEditDialogID.imagePath = fileSelectionDialogID.fileUrl;
                imageEditDialogID.isRemovable = false
                imageEditDialogID.open()
                break;
            case 2:
                videoEditDialogID.videoPath = fileSelectionDialogID.fileUrl;
                videoEditDialogID.isRemovable = false
                videoEditDialogID.open()
                break;
            case 3:
                fileShowDialogID.filePath = fileSelectionDialogID.fileUrl;
                fileShowDialogID.open()
                break;
            }
        }
    }

    ChatDetailAudioEditDialog{
        id:audioEditDialogID
        onSendAudio:print(path)
    }

    ChatDetailImageEditDialog{
        id:imageEditDialogID
        onSendImage:print(path)
    }

    ChatDetailVideoEditDialog{
        id:videoEditDialogID
        onSendVideo:print(path)
    }

    ChatDetailAudioRecordDialog{
        id:audioRecordDialogID
        onSendAudio: print(path)
        onEditAudio: {audioEditDialogID.audioPath = "file://"+path;audioEditDialogID.isRemovable = true;audioEditDialogID.open()}
    }

    ChatDetailImageCaptureDialog{
        id:imageCaptureDialogID
        onSendImage: print(path)
        onEditImage: {imageEditDialogID.imagePath = "file://"+path;imageEditDialogID.isRemovable = true;imageEditDialogID.open()}
    }

    ChatDetailVideoRecordDialog{
        id:videoRecordDialogID
        onSendVideo: print(path)
        onEditVideo: {videoEditDialogID.videoPath = path;videoEditDialogID.isRemovable = true;videoEditDialogID.open()}
    }

    ChatDetailFileShowDialog{
        id:fileShowDialogID
        onSendFile: print(path)
    }

    ChatDetailContactsListDialog{
        id:contactsListDialogID
        onSendContact: print(name+"---"+phone)
    }

    ChatDetailLocationShowDialog{
        id:locationShowDialogID
    }

    ChatEmojiesDialog{
        id:emojiDialogID
        onSelectEmoji: {textID.text += "<img src=\""+address+"\" width=\"20\" height=\"20\"/>";}
    }

}
