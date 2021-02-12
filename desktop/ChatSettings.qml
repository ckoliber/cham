import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.1
import Material 0.2
import Material.ListItems 0.1 as ListItem
import QtQuick.Controls 1.3 as QuickControls
import QtQuick.Controls.Material 2.0

Dialog {
    title: "Settings"
    width: if(parent.width <= 600){parent.width-20}else if(parent.width > 600 && parent.width <= 1000){(2*parent.width)/3}else{600}
    x: parent.width/2 - width/2
    y:parent.height/2 - height/2

    property int fontSize: 14

    View{
        id:userinterfaceID
        MouseArea{
            anchors.fill: parent
            hoverEnabled: true
            onHoveredChanged: if(containsMouse){userinterfaceID.state = "YH"}else{userinterfaceID.state = "NH"}
            cursorShape: "PointingHandCursor"
            onClicked: toggleItem(0);
        }

        states: [
            State {
                name: "NH"
                PropertyChanges {
                    target: userinterfaceID
                    backgroundColor: "#ddd"

                }
            },
            State {
                name: "YH"
                PropertyChanges {
                    target: userinterfaceID
                    backgroundColor: "#aaa"

                }
            }

        ]
        state: "NH"
        width: parent.width
        height: 50
        elevation: 1
        radius: 3
        Image{
            y:5
            x:5
            width: 40
            height: 40
            source: "qrc:///Icons/icons/material-ui.png"
        }
        Label{
            y:25 - height/2
            x: 60
            font.pixelSize: fontSize
            text:"User Interface"
        }

    }

    View{
        id:networkID
        MouseArea{
            anchors.fill: parent
            hoverEnabled: true
            onHoveredChanged: if(containsMouse){networkID.state = "YH"}else{networkID.state = "NH"}
            cursorShape: "PointingHandCursor"
            onClicked: toggleItem(1);
        }

        states: [
            State {
                name: "NH"
                PropertyChanges {
                    target: networkID
                    backgroundColor: "#ddd"

                }
            },
            State {
                name: "YH"
                PropertyChanges {
                    target: networkID
                    backgroundColor: "#aaa"

                }
            }
        ]
        state: "NH"
        width: parent.width
        height: 50
        elevation: 1
        radius: 3
        Image{
            y:5
            x:5
            width: 40
            height: 40
            source: "qrc:///Icons/icons/wan.png"
        }
        Label{
            y:parent.height/2 - height/2
            x: 60
            font.pixelSize: fontSize
            text:"Network"
        }

    }

    View{
        id:notificationsID
        MouseArea{
            anchors.fill: parent
            hoverEnabled: true
            onHoveredChanged: if(containsMouse){notificationsID.state = "YH"}else{notificationsID.state = "NH"}
            cursorShape: "PointingHandCursor"
            onClicked: toggleItem(2);
        }

        states: [
            State {
                name: "NH"
                PropertyChanges {
                    target: notificationsID
                    backgroundColor: "#ddd"

                }
            },
            State {
                name: "YH"
                PropertyChanges {
                    target: notificationsID
                    backgroundColor: "#aaa"

                }
            }
        ]
        state: "NH"
        width: parent.width
        height: 50
        elevation: 1
        radius: 3
        Image{
            y:5
            x:5
            width: 40
            height: 40
            source: "qrc:///Icons/icons/notification-clear-all.png"
        }

        Label{
            y:parent.height/2 - height/2
            x: 60
            font.pixelSize: fontSize
            text:"Notifications"
        }

    }

    View{
        id:cacheID
        MouseArea{
            anchors.fill: parent
            hoverEnabled: true
            onHoveredChanged: if(containsMouse){cacheID.state = "YH"}else{cacheID.state = "NH"}
            cursorShape: "PointingHandCursor"
            onClicked: toggleItem(3);
        }
        states: [
            State {
                name: "NH"
                PropertyChanges {
                    target: cacheID
                    backgroundColor: "#ddd"

                }
            },
            State {
                name: "YH"
                PropertyChanges {
                    target: cacheID
                    backgroundColor: "#aaa"

                }
            }
        ]
        state: "NH"
        width: parent.width
        height: 50
        elevation: 1
        radius: 3
        Image{
            y:5
            x:5
            width: 40
            height: 40
            source: "qrc:///Icons/icons/cached.png"
        }

        Label{
            y:parent.height/2 - height/2
            x: 60
            font.pixelSize: fontSize
            text:"Cache"
        }

    }

    View{
        id:securityID
        MouseArea{
            anchors.fill: parent
            hoverEnabled: true
            onHoveredChanged: if(containsMouse){securityID.state = "YH"}else{securityID.state = "NH"}
            cursorShape: "PointingHandCursor"
            onClicked: toggleItem(4);
        }

        states: [
            State {
                name: "NH"
                PropertyChanges {
                    target: securityID
                    backgroundColor: "#ddd"

                }
            },
            State {
                name: "YH"
                PropertyChanges {
                    target: securityID
                    backgroundColor: "#aaa"

                }
            }
        ]
        state: "NH"
        width: parent.width
        height: 50
        elevation: 1
        radius: 3
        Image{
            y:5
            x:5
            width: 40
            height: 40
            source: "qrc:///Icons/icons/security.png"
        }

        Label{
            y:parent.height/2 - height/2
            x: 60
            font.pixelSize: fontSize
            text:"Security"
        }

    }


    Component.onCompleted: {
        //backgroundsModel.append({"img":"qrc:///Data/data/icon.png"})
        //backgroundsModel.append({"img":"qrc:///Data/data/KoLiBer.png"})
    }

    function toggleItem(num){
        switch(num){
        case 0:
            userinterfaceDialogID.open()
            break;
        case 1:
            networkDialogID.open()
            break;
        case 2 :
            notificationsDialogID.open()
            break;
        case 3:
            cacheDialogID.open()
            break;
        case 4:
            securityDialogID.open()
            break;
        }
    }

    function setGridType(type){
        switch(type){
        case 0:
            // list
            break;
        case 1:
            // grid
            break;
        }

    }

    function setThemeColor(type,clr){
        switch(type) {
            case 0:
                theme.primaryColor = clr
                // change theme at DB
                break;
            case 1:
                theme.accentColor = clr
                // change theme at DB
                break;
        }
    }

    function setBackgroundImage(image){
        // change background image
    }

    function setSetting(key,value){
        switch(key){
        case "AUTOUPDATE":
            break;
        }
    }

    function clearData(key){
        // clear data and refresh labels
        switch(key){
        case "IMG":
            break;
        case "AUD":
            break;
        case "VDO":
            break;
        case "FIL":
            break;
        }
    }


    Dialog {
        id: colorPickerID
        title: "Set color"
        positiveButtonText: "Done"
        MenuField {
            id: selection
            model: ["Primary color", "Accent color"]
            width: dp(160)
        }

        Grid {
            columns: 7
            spacing: dp(8)

            ListModel{
                id:colorsModel
                ListElement{name:"red"}
                ListElement{name:"pink"}
                ListElement{name:"purple"}
                ListElement{name:"deepPurple"}
                ListElement{name:"indigo"}
                ListElement{name:"blue"}
                ListElement{name:"lightBlue"}
                ListElement{name:"cyan"}
                ListElement{name:"teal"}
                ListElement{name:"green"}
                ListElement{name:"lime"}
                ListElement{name:"lightGreen"}
                ListElement{name:"yellow"}
                ListElement{name:"amber"}
                ListElement{name:"orange"}
                ListElement{name:"deepOrange"}
                ListElement{name:"grey"}
                ListElement{name:"blueGrey"}
                ListElement{name:"brown"}
            }

            Repeater {
                model: colorsModel
                Rectangle {
                    width: dp(30)
                    height: dp(30)
                    radius: dp(2)
                    color: Palette.colors[modelData]["500"]
                    border.width: modelData === "white" ? dp(2) : 0
                    border.color: Theme.alpha("#000", 0.26)

                    Ink {
                        anchors.fill: parent
                        onPressed: {
                            setThemeColor(selection.selectedIndex,colorsModel.get(index).name)
                        }
                    }
                }
            }
        }
    }

    ListModel{
        id:backgroundsModel
        ListElement {
            imgCode: "Jim Williams"
            imgSource: "qrc:///Data/data/icon.png"
        }
        ListElement {
            imgCode: "John Brown"
            imgSource: "qrc:///Data/data/balloon.jpg"
        }
        ListElement {
            imgCode: "Bill Smyth"
            imgSource: "qrc:///Data/data/KoLiBer.png"
        }
        ListElement {
            imgCode: "Sam Wise"
            imgSource: "qrc:///Data/data/icon.png"
        }
    }

    Dialog {
        id: chatBackgroundID
        title : "Chat Background"
        width: if(parent.width <= 600){parent.width-20}else if(parent.width > 600 && parent.width <= 1000){(2*parent.width)/3}else{600}
        positiveButtonText: "Done"
        negativeButtonText: "Cancel"

        Button{
            text:"From Local"
            onClicked: actionSheet.open()
        }

        Grid {
            y:50
            columns: 5
            width: parent.width

            Repeater {
                model: backgroundsModel
                Image {
                    sourceSize.width: 150
                    sourceSize.height: 150
                    width: 70
                    height: 70
                    source: imgSource
                }
            }


        }

    }

    Dialog {
        id: gridTypeID
        title : "Grid Type"
        width: 300
        height: 200
        x: parent.width/2 - width/2
        y:parent.height/2 - height/2
        Rectangle{
            anchors.fill: parent

            Row{
                anchors.centerIn: parent
                spacing: 30
                Image{
                    Ink{anchors.fill: parent ; onClicked: setGridType(0)}
                    y:35
                    x:parent.width - width - 10
                    width: 50
                    height: 50
                    source: "qrc:///Icons/icons/view-list.png"
                }

                Image{
                    Ink{anchors.fill: parent ; onClicked: setGridType(1)}
                    y:35
                    x:parent.width - width - 10
                    width: 50
                    height: 50
                    source: "qrc:///Icons/icons/view-module.png"
                }

            }

        }


    }

    Dialog{
        title: "User Interface"
        id:userinterfaceDialogID
        width: if(parent.width <= 600){parent.width-20}else if(parent.width > 600 && parent.width <= 1000){(2*parent.width)/3}else{600}
        x: parent.width/2 - width/2
        y:parent.height/2 - height/2

        View{
            backgroundColor: "#ddd"
            width: parent.width
            height: 50
            elevation: 1
            radius: 3
            Image{
                y:5
                x:5
                width: 40
                height: 40
                source: "qrc:///Icons/icons/palette.png"
            }
            Label{
                y:25 - height/2
                x: 60
                font.pixelSize: fontSize
                text:"Colorize : "
            }

            Button{
                text:"Set Color"
                y:parent.height/2 - height/2
                x: parent.width - width - 10
                onClicked: colorPickerID.open()
            }

        }

        View{
            backgroundColor: "#ddd"
            width: parent.width
            height: 50
            elevation: 1
            radius: 3
            Image{
                y:5
                x:5
                width: 40
                height: 40
                source: "qrc:///Icons/icons/image-filter-hdr.png"
            }
            Label{
                y:25 - height/2
                x: 60
                font.pixelSize: fontSize
                text:"Chat Background : "
            }
            Image{
                Ink{anchors.fill: parent ; onClicked: chatBackgroundID.open()}
                y:5
                x:parent.width - width - 10
                width: 40
                height: 40
                source: "qrc:///Data/data/icon.png"
            }

        }

        View{
            backgroundColor: "#ddd"
            width: parent.width
            height: 50
            elevation: 1
            radius: 3
            Image{
                y:5
                x:5
                width: 40
                height: 40
                source: "qrc:///Icons/icons/view-grid.png"
            }
            Label{
                y:25 - height/2
                x: 60
                font.pixelSize: fontSize
                text:"Grid Type : "
            }
            Image{
                Ink{anchors.fill: parent ; onClicked: gridTypeID.open()}
                y:5
                x:parent.width - width - 10
                width: 40
                height: 40
                source: "qrc:///Icons/icons/view-list.png"
            }

        }


    }

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

    Dialog{
        title: "Network"
        id:networkDialogID
        width: if(parent.width <= 600){parent.width-20}else if(parent.width > 600 && parent.width <= 1000){(2*parent.width)/3}else{600}
        x: parent.width/2 - width/2
        y:parent.height/2 - height/2

        View{
            width: parent.width
            height: 50
            backgroundColor: "#ddd"
            elevation: 1
            radius: 3
            Image{
                y:5
                x:5
                width: 40
                height: 40
                source: "qrc:///Icons/icons/update.png"
            }
            Label{
                y:25 - height/2
                x: 60
                font.pixelSize: fontSize
                text:"Auto Update : "
            }

            Switch{
                y:parent.height/2 - height/2
                x: parent.width - width - 10
                onClicked: setSetting("AUTOUPDATE",checked);
            }

        }

        View{
            backgroundColor: "#bbb"
            width: parent.width
            height: 50
            elevation: 1
            radius: 3
            Ink{anchors.fill: parent;onClicked: autodownloadDialogID.open();cursorShape: "PointingHandCursor"}
            Image{
                y:10
                x:10
                width: 30
                height: 30
                source: "qrc:///Icons/icons/download.png"
            }
            Label{
                y:25 - height/2
                x: 60
                font.pixelSize: fontSize
                text:"Automatic Media Download"
            }


        }

    }

    Dialog{
        title: "Automatic Media Download"
        id:autodownloadDialogID
        width: if(parent.width <= 600){parent.width-20}else if(parent.width > 600 && parent.width <= 1000){(2*parent.width)/3}else{600}
        x: parent.width/2 - width/2
        y:parent.height/2 - height/2
        Column{
            width: parent.width
            height: 150
            spacing: 10
            Row{
                width: parent.width
                CheckBox{
                    checked: false
                    text:"Pictures"
                    onClicked: setSetting("AUTOIDL",checked);
                }
            }
            Row{
                width: parent.width
                CheckBox{
                    checked: false
                    text:"Audios"
                    onClicked: setSetting("AUTOADL",checked);
                }
            }
            Row{
                width: parent.width
                CheckBox{
                    checked: false
                    text:"Videos"
                    onClicked: setSetting("AUTOVDL",checked);
                }
            }

        }


    }

    Dialog{
        title: "Notifications"
        id:notificationsDialogID
        width: if(parent.width <= 600){parent.width-20}else if(parent.width > 600 && parent.width <= 1000){(2*parent.width)/3}else{600}
        x: parent.width/2 - width/2
        y:parent.height/2 - height/2

        View{
            id:messagenotifyView
            backgroundColor: "#ddd"
            width: parent.width
            state: "HIDE"
            elevation: 1
            radius: 3
            Label{
                y:25 - height/2
                x: 10
                font.pixelSize: fontSize
                text:"Message notifications : "
            }

            Switch{
                y:15
                x: parent.width - width - 10
                onClicked: {setSetting("MSGNOTIFY",checked);if(checked){messagenotifyView.state = "SHOW"}else{messagenotifyView.state = "HIDE"}}
            }

            Column{
                id:messagenotifycontentView
                y:60
                visible: true
                width: parent.width
                View{
                    width: parent.width
                    height: 50
                    Label{
                        font.pixelSize: fontSize
                        x:20
                        text: "Notifications popup : "
                    }

                    Switch{
                        x:parent.width - width - 20

                    }
                }

                View{
                    width: parent.width
                    height: 50
                    Label{
                        font.pixelSize: fontSize
                        x:20
                        text: "Notifications tone : "
                    }

                    Switch{
                        x:parent.width - width - 20

                    }
                }

            }

            states: [
                State {
                    name: "SHOW"
                    PropertyChanges {
                        target: messagenotifyView
                        height: 150
                    }
                    PropertyChanges {
                        target: messagenotifycontentView
                        visible: true

                    }
                },
                State {
                    name: "HIDE"
                    PropertyChanges {
                        target: messagenotifyView
                        height: 50

                    }
                    PropertyChanges {
                        target: messagenotifycontentView
                        visible: false

                    }
                }
            ]

        }

        View{
            id:gpchnotifyView
            backgroundColor: "#ddd"
            width: parent.width
            state: "HIDE"
            elevation: 1
            radius: 3
            Label{
                y:25 - height/2
                x: 10
                font.pixelSize: fontSize
                text:"Group and channel notifications : "
            }

            Switch{
                y:15
                x: parent.width - width - 10
                onClicked: {setSetting("GPCHNOTIFY",checked);if(checked){gpchnotifyView.state = "SHOW"}else{gpchnotifyView.state = "HIDE"}}
            }

            Column{
                id:gpchnotifycontentView
                y:60
                visible: true
                width: parent.width
                View{
                    width: parent.width
                    height: 50
                    Label{
                        font.pixelSize: fontSize
                        x:20
                        text: "Notifications popup : "
                    }

                    Switch{
                        x:parent.width - width - 20

                    }
                }

                View{
                    width: parent.width
                    height: 50
                    Label{
                        font.pixelSize: fontSize
                        x:20
                        text: "Notifications tone : "
                    }

                    Switch{
                        x:parent.width - width - 20

                    }
                }

            }

            states: [
                State {
                    name: "SHOW"
                    PropertyChanges {
                        target: gpchnotifyView
                        height: 150

                    }
                    PropertyChanges {
                        target: gpchnotifycontentView
                        visible: true

                    }
                },
                State {
                    name: "HIDE"
                    PropertyChanges {
                        target: gpchnotifyView
                        height: 50

                    }
                    PropertyChanges {
                        target: gpchnotifycontentView
                        visible: false

                    }
                }
            ]

        }

        View{
            backgroundColor: "#ddd"
            id:callnotifyView
            width: parent.width
            state: "HIDE"
            elevation: 1
            radius: 3
            Label{
                y:25 - height/2
                x: 10
                font.pixelSize: fontSize
                text:"Call notifications : "
            }

            Switch{
                y:15
                x: parent.width - width - 10
                onClicked: {setSetting("CALLNOTIFY",checked);if(checked){callnotifyView.state = "SHOW"}else{callnotifyView.state = "HIDE"}}
            }

            Column{
                id:callnotifycontentView
                y:60
                visible: true
                width: parent.width
                View{
                    width: parent.width
                    height: 50
                    Label{
                        font.pixelSize: fontSize
                        x:20
                        text: "Notifications popup : "
                    }

                    Switch{
                        x:parent.width - width - 20

                    }
                }

                View{
                    width: parent.width
                    height: 50
                    Label{
                        font.pixelSize: fontSize
                        x:20
                        text: "Notifications tone : "
                    }

                    Switch{
                        x:parent.width - width - 20

                    }
                }

            }

            states: [
                State {
                    name: "SHOW"
                    PropertyChanges {
                        target: callnotifyView
                        height: 150

                    }
                    PropertyChanges {
                        target: callnotifycontentView
                        visible: true

                    }
                },
                State {
                    name: "HIDE"
                    PropertyChanges {
                        target: callnotifyView
                        height: 50

                    }
                    PropertyChanges {
                        target: callnotifycontentView
                        visible: false

                    }
                }
            ]

        }

    }

    Dialog{
        title: "Cache"
        id:cacheDialogID
        width: if(parent.width <= 600){parent.width-20}else if(parent.width > 600 && parent.width <= 1000){(2*parent.width)/3}else{600}
        x: parent.width/2 - width/2
        y:parent.height/2 - height/2

        View{
            width: parent.width
            height: 50

            Label{
                y:25 - height/2
                x: 5
                font.pixelSize: fontSize
                font.weight: Font.Bold
                text:"Images : "
            }

            Label{
                id:cacheDialogImagesLabel
                y:25 - height/2
                x:parent.width/2 - width/2
                font.pixelSize: fontSize
                text:"0 B"
            }

            Image{
                y:25 - height/2
                x: parent.width - width - 10
                width: 30
                height: 30
                source: "qrc:///Icons/icons/delete.png"
                Ink{
                    anchors.fill: parent
                    cursorShape: "PointingHandCursor"
                    onClicked: clearData("IMG")
                }
            }

        }

        View{
            width: parent.width
            height: 50

            Label{
                y:25 - height/2
                x: 5
                font.pixelSize: fontSize
                font.weight: Font.Bold
                text:"Audios : "
            }

            Label{
                id:cacheDialogAudiosLabel
                y:25 - height/2
                x:parent.width/2 - width/2
                font.pixelSize: fontSize
                text:"0 B"
            }

            Image{
                y:25 - height/2
                x: parent.width - width - 10
                width: 30
                height: 30
                source: "qrc:///Icons/icons/delete.png"
                Ink{
                    anchors.fill: parent
                    cursorShape: "PointingHandCursor"
                    onClicked: clearData("AUD")
                }
            }

        }

        View{
            width: parent.width
            height: 50

            Label{
                y:25 - height/2
                x: 5
                font.pixelSize: fontSize
                font.weight: Font.Bold
                text:"Videos : "
            }

            Label{
                id:cacheDialogVideosLabel
                y:25 - height/2
                x:parent.width/2 - width/2
                font.pixelSize: fontSize
                text:"0 B"
            }

            Image{
                y:25 - height/2
                x: parent.width - width - 10
                width: 30
                height: 30
                source: "qrc:///Icons/icons/delete.png"
                Ink{
                    anchors.fill: parent
                    cursorShape: "PointingHandCursor"
                    onClicked: clearData("VDO")
                }
            }

        }

        View{
            width: parent.width
            height: 50

            Label{
                y:25 - height/2
                x: 5
                font.pixelSize: fontSize
                font.weight: Font.Bold
                text:"Files : "
            }

            Label{
                id:cacheDialogFilesLabel
                y:25 - height/2
                x:parent.width/2 - width/2
                font.pixelSize: fontSize
                text:"0 B"
            }

            Image{
                y:25 - height/2
                x: parent.width - width - 10
                width: 30
                height: 30
                source: "qrc:///Icons/icons/delete.png"
                Ink{
                    anchors.fill: parent
                    cursorShape: "PointingHandCursor"
                    onClicked: clearData("FIL")
                }
            }

        }


    }

    Dialog{
        title: "Security"
        id:securityDialogID
        width: if(parent.width <= 600){parent.width-20}else if(parent.width > 600 && parent.width <= 1000){(2*parent.width)/3}else{600}
        x: parent.width/2 - width/2
        y:parent.height/2 - height/2

        View{
            id:securityDialogCSCView
            backgroundColor: "#ddd"
            width: parent.width
            state:"HIDE"
            elevation: 1
            radius: 3
            Label{
                y:25 - height/2
                x: 10
                font.pixelSize: fontSize
                text:"ChaM Security Code : "
            }

            CheckBox{
                id:securityDialogCSCViewCheckbox
                y:25 - height/2
                x: parent.width - width - 10
                onClicked: if(checked){securityDialogCSCViewCheckbox.checked = false;securityenterpassDialogID.open()}else{securityDialogCSCView.state = "HIDE"}
            }

            View{
                id:securityDialogCSCTime
                y:50
                width: parent.width
                height: 50
                Label{
                    y:25 - height/2
                    x: 10
                    font.pixelSize: fontSize
                    text:"Enable password after : "

                }
                MenuField{
                    y:25 - height/2
                    x: parent.width - width - 10
                    model:["1 min","10 min","1 hour","5 hour"]
                }
            }

            states:[
                State{
                    name: "SHOW"
                    PropertyChanges {
                        target: securityDialogCSCTime
                        visible:true
                    }
                    PropertyChanges {
                        target: securityDialogCSCView
                        height:100
                    }
                    PropertyChanges {
                        target: securityDialogCSCViewCheckbox
                        checked: true
                    }
                },
                State{
                    name: "HIDE"
                    PropertyChanges {
                        target: securityDialogCSCTime
                        visible:false
                    }
                    PropertyChanges {
                        target: securityDialogCSCView
                        height:50
                    }
                    PropertyChanges {
                        target: securityDialogCSCViewCheckbox
                        checked: false
                    }

                }

            ]

        }

        View{
            backgroundColor: "#ddd"
            width: parent.width
            height: 50
            elevation: 1
            radius: 3
            Label{
                y:parent.height/2 - height/2
                x: 10
                font.pixelSize: fontSize
                text:"Show all sessions : "
            }

            Button{
                y:parent.height/2 - height/2
                x: parent.width - width - 10
                text:"Show"
                onClicked: securitysessionsDialogID.open()
            }

        }


    }

    Dialog{
        id:securityenterpassDialogID
        title: "Enter Password"
        width: if(parent.width <= 600){parent.width-20}else if(parent.width > 600 && parent.width <= 1000){(2*parent.width)/3}else{600}
        x: parent.width/2 - width/2
        y:parent.height/2 - height/2
        hasActions: false
        View{
            width: parent.width
            height: 50
            TextField {
                anchors.centerIn: parent
                id: passwordFieldCSCTextFieldID
                placeholderText: "Password"
                floatingLabel: true
                echoMode: TextInput.Password
                anchors.horizontalCenter: parent.horizontalCenter
                hasError: false
                onTextChanged: if(passwordFieldCSCTextFieldID.text != ""){passwordFieldCSCTextFieldID.hasError = false}else{passwordFieldCSCTextFieldID.hasError = true}
            }

        }
        View{
            width: parent.width
            height: 50
            TextField {
                anchors.centerIn: parent
                id:passwordretypeFieldCSCTextFieldID
                placeholderText: "Retype password"
                floatingLabel: true
                echoMode: TextInput.Password
                anchors.horizontalCenter: parent.horizontalCenter
                onFocusChanged: if(focus){if(passwordretypeFieldCSCTextFieldID.text == passwordFieldCSCTextFieldID.text){passwordretypeFieldCSCTextFieldID.hasError = false}else{passwordretypeFieldCSCTextFieldID.hasError = true}}
                onTextChanged: if(passwordretypeFieldCSCTextFieldID.text != "" && passwordretypeFieldCSCTextFieldID.text == passwordFieldCSCTextFieldID.text){passwordretypeFieldCSCTextFieldID.hasError = false}else{passwordretypeFieldCSCTextFieldID.hasError = true}
            }

        }
        Row{
            x:parent.width - width
            Button{
                text: "Cancel"
                onClicked: securityenterpassDialogID.close()
            }
            Button{
                text: "Ok"
                onClicked: if(passwordretypeFieldCSCTextFieldID.text != "" && passwordretypeFieldCSCTextFieldID.text == passwordFieldCSCTextFieldID.text){securityDialogCSCView.state = "SHOW";securityenterpassDialogID.close();snackbar.open("Passwords sets !")}else{snackbar.open("Passwords does not matched !")}
            }
        }
    }

    ListModel{id:securitysessionslistmodelID}

    Dialog{
        id:securitysessionsDialogID
        title:"Sessions"
        width: if(parent.width <= 600){parent.width-20}else if(parent.width > 600 && parent.width <= 1000){(2*parent.width)/3}else{600}
        x: parent.width/2 - width/2
        y:parent.height/2 - height/2
        ListView{
            width: parent.width
            model: securitysessionslistmodelID
            delegate: Row{
                anchors.fill: parent
                /////
            }
        }
    }



}
