import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.0
import QtGraphicalEffects 1.0
import Material 0.2
import Material.ListItems 0.1 as ListItem

ApplicationWindow {
    visible: true
    width: 640
    height: 480
    title: qsTr("Hello World")
    minimumHeight: 450
    minimumWidth: 350

    function messageReceived(name){
        print(name+"-----");
    }

    function onNavItemSelected(title){
        switch(title){
        case "new Group":
            newgroup.open()
            break;
        case "new Channel":
            newchannel.open()
            break;
        case "Search By ID":
            search.open()
            break;
        case "Settings":
            settings.open()
            break;
        case "My Account":
            myaccount.open()
            break;
        case "About KoLiBer":
            koliber.open()
            break;
        }
    }

    ChatAboutKoLiBer{
        id:koliber
    }

    ChatSearch{
        id:search
    }

    ChatSettings{
        id:settings
    }

    ChatMyAccount{
        id:myaccount
    }

    ChatNewGroup{
        id:newgroup
    }

    ChatNewChannel{
        id:newchannel
    }

    ChatAccount{
        id:account
    }

    theme {
        primaryColor: "blue"
        accentColor: "white"
        backgroundColor: "white"
    }

    ListModel{
        id:makeComponent
        ListElement{name:"new Group";icon:"qrc:///Icons/icons/group.png"}
        ListElement{name:"new Channel";icon:"qrc:///Icons/icons/channel.png"}
        ListElement{name:"Search By ID";icon:"qrc:///Icons/icons/search.png"}
    }

    ListModel{
        id:meComponent
        ListElement{name:"Settings";icon:"qrc:///Icons/icons/settings.png"}
        ListElement{name:"My Account";icon:"qrc:///Icons/icons/account.png"}
    }

    ListModel{
        id:faqComponent
        ListElement{name:"About KoLiBer";icon:"qrc:///Icons/icons/about.png"}
    }

    property var sections: [ makeComponent, meComponent, faqComponent ]

    property var sectionTitles: [ "", "Me", "FAQ" ]

    property string selectedComponent: sections[0].get(0).name

    initialPage: TabbedPage {
        id: page
        title: "ChaM"
        actions: [
            Action {
                iconSource: "qrc:///Icons/icons/world.png"
                name: "Global Sessions"
                hoverAnimation: true
                onTriggered: print("AAA")
            },

            Action {
                iconSource: "qrc:///Icons/icons/account.png"
                name: "My Account"
                hoverAnimation: true
                onTriggered: account.open()
            },
            Action {
                iconSource: "qrc:///Icons/icons/phone.png"
                name: "Call"
                visible: false
                hoverAnimation: true
                onTriggered: account.open()
            }
        ]
        backAction: navDrawer.action
        NavigationDrawer {
            id: navDrawer
            Flickable {
                anchors.fill: parent
                contentHeight: Math.max(content.implicitHeight, height)
                Column {
                    id: content
                    anchors.fill: parent

                    Item {
                        width: parent.width
                        height: dp(150)

                        LinearGradient {
                            anchors.fill: parent
                            start: Qt.point(parent.width, 0)
                            end: Qt.point(0, 0)
                            gradient: Gradient {
                                GradientStop { position: 0.0; color: Palette.colors[theme.primaryColor]["200"] }
                                GradientStop { position: 1.0; color: Palette.colors[theme.primaryColor]["500"] }
                            }
                        }

                        Row{
                            y: parent.height - height -5
                            x: 7
                            Icon {
                                color: "#333"
                                size: dp(40)
                                id: mainUserPicture
                                source: "qrc:///Icons/icons/account.png"

                            }
                            Column{
                                y:7
                                x:10
                                Text {
                                    x: 10
                                    id:mainUserNameText
                                    color: "white"
                                    text: qsTr("ali")
                                }

                                Text {
                                    x: 10
                                    id:mainUserPhone
                                    color: "#eee"
                                    text: qsTr("+989380851109")
                                }
                            }


                        }



                    }

                    Repeater {
                        model: sections
                        delegate: Column {
                            width: parent.width
                            ListItem.Divider{
                                visible: if(index > 0 ) true
                                         else false

                            }
                            ListItem.Subheader {
                                visible: if(index > 0 ) true
                                         else false
                                text: sectionTitles[index]
                            }

                            Repeater {
                                model: modelData
                                delegate: ListItem.Standard {
                                    iconSource: modelData.get(index).icon
                                    text:modelData.get(index).name
                                    onClicked: {
                                        navDrawer.close()
                                        onNavItemSelected(modelData.get(index).name)

                                    }
                                }
                            }
                        }
                    }
                }

            }
        }

        Loader {
            anchors.fill: parent
            sourceComponent: mainComponent
        }
    }

    Component {
        id: mainComponent
        ChatPanel{}

    }

    OverlayView {
        id: overlayView
        width: dp(800)
        height: dp(500)
        Image {
            id: contentImage
            source: Qt.resolvedUrl("qrc:///Data/data/KoLiBer.png")
            anchors.fill: parent
        }


    }

    Snackbar {
        id: snackbar
    }
}
