import QtQuick 2.0
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.1
import Material 0.2
import Material.ListItems 0.1 as ListItem
import QtQuick.Controls.Material 2.0

Popup {

    signal selectEmoji(string address);
    padding: 0
    width: 320
    height: 300
    margins: 0
    x:10
    y:parent.height - height - 60
    Component.onCompleted: {
        var xhr = new XMLHttpRequest;
        xhr.open("GET", "qrc:///Emoji/emoji/emoji.json");
        xhr.onreadystatechange = function() {
            if (xhr.readyState == XMLHttpRequest.DONE) {
                var list = JSON.parse(xhr.responseText);
                for(var item in list){
                    if(list[item]["category"] == "people"){
                        emojiesModel1.append({iconSource:"qrc:///Emoji/emoji/png/"+list[item]["unicode"]+".png",shortName:list[item]["shortname"]})
                    }else if(list[item]["category"] == "food"){
                        emojiesModel2.append({iconSource:"qrc:///Emoji/emoji/png/"+list[item]["unicode"]+".png",shortName:list[item]["shortname"]})
                    }else if(list[item]["category"] == "nature"){
                        emojiesModel3.append({iconSource:"qrc:///Emoji/emoji/png/"+list[item]["unicode"]+".png",shortName:list[item]["shortname"]})
                    }else if(list[item]["category"] == "activity"){
                        emojiesModel4.append({iconSource:"qrc:///Emoji/emoji/png/"+list[item]["unicode"]+".png",shortName:list[item]["shortname"]})
                    }else if(list[item]["category"] == "objects"){
                        emojiesModel5.append({iconSource:"qrc:///Emoji/emoji/png/"+list[item]["unicode"]+".png",shortName:list[item]["shortname"]})
                    }else if(list[item]["category"] == "travel"){
                        emojiesModel7.append({iconSource:"qrc:///Emoji/emoji/png/"+list[item]["unicode"]+".png",shortName:list[item]["shortname"]})
                    }else if(list[item]["category"] == "flags"){
                        emojiesModel8.append({iconSource:"qrc:///Emoji/emoji/png/"+list[item]["unicode"]+".png",shortName:list[item]["shortname"]})
                    }else{
                        emojiesModel6.append({iconSource:"qrc:///Emoji/emoji/png/"+list[item]["unicode"]+".png",shortName:list[item]["shortname"]})
                    }
                }
            }
        }
        xhr.send();
        var recemj = SIGNALHANDLER.recentEmojies("").split("|");
        for(var a = 0; a < recemj.length; a++){
            emojiesModel0.append({iconSource:recemj[a].split("@")[0],shortName:recemj[a].split("@")[1]})
        }
    }

    function sendEmoji(emojiSource,emojiShortcut){
        SIGNALHANDLER.recentEmojies(emojiSource+"@"+emojiShortcut);
        selectEmoji(emojiSource)
        emojiesModel0.clear()
        var recemj = SIGNALHANDLER.recentEmojies("").split("|");
        for(var a = 0; a < recemj.length; a++){
            emojiesModel0.append({iconSource:recemj[a].split("@")[0],shortName:recemj[a].split("@")[1]})
        }
    }

    ListModel{id:emojiesModel0}
    ListModel{id:emojiesModel1}
    ListModel{id:emojiesModel2}
    ListModel{id:emojiesModel3}
    ListModel{id:emojiesModel4}
    ListModel{id:emojiesModel5}
    ListModel{id:emojiesModel6}
    ListModel{id:emojiesModel7}
    ListModel{id:emojiesModel8}

    SwipeView {
        id: emojiesSwipeViewID
        width: parent.width
        height: parent.height-50
        currentIndex: emojiesButtonsRowID.activeIndex
        onCurrentIndexChanged: emojiesButtonsRowID.activeIndex = currentIndex
        Item{
            visible: emojiesSwipeViewID.currentIndex == 0
            GridView{
                ScrollBar.vertical: ScrollBar {}
                id: view0
                anchors.margins: 10
                anchors.fill: parent
                cellHeight: 40
                cellWidth: cellHeight
                model: emojiesModel0
                clip: true
                delegate: Item {
                    height: view0.cellHeight
                    width: view0.cellWidth
                    Image{
                        width: 25
                        height: 25
                        anchors.centerIn: parent
                        sourceSize.width: 35
                        sourceSize.height: 35
                        source: model.iconSource
                        Ink{
                            anchors.fill: parent
                            onClicked: sendEmoji(model.iconSource,model.shortName)
                        }
                    }
                }
            }

        }

        Item{
            visible: emojiesSwipeViewID.currentIndex == 1
            GridView{
                ScrollBar.vertical: ScrollBar {}
                id: view1
                anchors.margins: 10
                anchors.fill: parent
                cellHeight: 40
                cellWidth: cellHeight
                model: emojiesModel1
                clip: true
                delegate: Item {
                    height: view1.cellHeight
                    width: view1.cellWidth
                    Image{
                        width: 25
                        height: 25
                        anchors.centerIn: parent
                        sourceSize.width: 35
                        sourceSize.height: 35
                        source: model.iconSource
                        Ink{
                            anchors.fill: parent
                            onClicked: sendEmoji(model.iconSource,model.shortName)
                        }
                    }
                }
            }

        }

        Item{
            visible: emojiesSwipeViewID.currentIndex == 2

            GridView{
                ScrollBar.vertical: ScrollBar {}
                id: view2
                anchors.margins: 10
                anchors.fill: parent
                cellHeight: 40
                cellWidth: cellHeight
                model: emojiesModel2
                clip: true
                delegate: Item {
                    property var view: GridView.view
                    height: view2.cellHeight
                    width: view2.cellWidth
                    Image{
                        width: 25
                        height: 25
                        anchors.centerIn: parent
                        sourceSize.width: 35
                        sourceSize.height: 35
                        source: model.iconSource
                        Ink{
                            anchors.fill: parent
                            onClicked: sendEmoji(model.iconSource,model.shortName)
                        }
                    }
                }
            }

        }

        Item{
            visible: emojiesSwipeViewID.currentIndex == 3

            GridView{
                ScrollBar.vertical: ScrollBar {}
                id: view3
                anchors.margins: 10
                anchors.fill: parent
                cellHeight: 40
                cellWidth: cellHeight
                model: emojiesModel3
                clip: true
                delegate: Item {
                    height: view3.cellHeight
                    width: view3.cellWidth
                    Image{
                        width: 25
                        height: 25
                        anchors.centerIn: parent
                        sourceSize.width: 35
                        sourceSize.height: 35
                        source: model.iconSource
                        Ink{
                            anchors.fill: parent
                            onClicked: sendEmoji(model.iconSource,model.shortName)
                        }
                    }
                }
            }

        }

        Item{
            visible: emojiesSwipeViewID.currentIndex == 4

            GridView{
                ScrollBar.vertical: ScrollBar {}
                id: view4
                anchors.margins: 10
                anchors.fill: parent
                cellHeight: 40
                cellWidth: cellHeight
                model: emojiesModel4
                clip: true
                delegate: Item {
                    height: view4.cellHeight
                    width: view4.cellWidth
                    Image{
                        width: 25
                        height: 25
                        anchors.centerIn: parent
                        sourceSize.width: 35
                        sourceSize.height: 35
                        source: model.iconSource
                        Ink{
                            anchors.fill: parent
                            onClicked: sendEmoji(model.iconSource,model.shortName)
                        }
                    }
                }
            }

        }

        Item{
            visible: emojiesSwipeViewID.currentIndex == 5

            GridView{
                ScrollBar.vertical: ScrollBar {}
                id: view5
                anchors.margins: 10
                anchors.fill: parent
                cellHeight: 40
                cellWidth: cellHeight
                model: emojiesModel5
                clip: true
                delegate: Item {
                    height: view5.cellHeight
                    width: view5.cellWidth
                    Image{
                        width: 25
                        height: 25
                        anchors.centerIn: parent
                        sourceSize.width: 35
                        sourceSize.height: 35
                        source: model.iconSource
                        Ink{
                            anchors.fill: parent
                            onClicked: sendEmoji(model.iconSource,model.shortName)
                        }
                    }
                }
            }

        }

        Item{
            visible: emojiesSwipeViewID.currentIndex == 6
            GridView{
                ScrollBar.vertical: ScrollBar {}
                id: view6
                anchors.margins: 10
                anchors.fill: parent
                cellHeight: 40
                cellWidth: cellHeight
                model: emojiesModel6
                clip: true
                delegate: Item {
                    height: view6.cellHeight
                    width: view6.cellWidth
                    Image{
                        width: 25
                        height: 25
                        anchors.centerIn: parent
                        sourceSize.width: 35
                        sourceSize.height: 35
                        source: model.iconSource
                        Ink{
                            anchors.fill: parent
                            onClicked: sendEmoji(model.iconSource,model.shortName)
                        }
                    }
                }
            }

        }

        Item{
            visible: emojiesSwipeViewID.currentIndex == 7

            GridView{
                ScrollBar.vertical: ScrollBar {}
                id: view7
                anchors.margins: 10
                anchors.fill: parent
                cellHeight: 40
                cellWidth: cellHeight
                model: emojiesModel7
                clip: true
                delegate: Item {
                    height: view7.cellHeight
                    width: view7.cellWidth
                    Image{
                        width: 25
                        height: 25
                        anchors.centerIn: parent
                        sourceSize.width: 35
                        sourceSize.height: 35
                        source: model.iconSource
                        Ink{
                            anchors.fill: parent
                            onClicked: sendEmoji(model.iconSource,model.shortName)
                        }
                    }
                }
            }

        }

        Item{
            visible: emojiesSwipeViewID.currentIndex == 8

            GridView{
                ScrollBar.vertical: ScrollBar {}
                id: view8
                anchors.margins: 10
                anchors.fill: parent
                cellHeight: 40
                cellWidth: cellHeight
                model: emojiesModel8
                clip: true
                delegate: Item {
                    height: view8.cellHeight
                    width: view8.cellWidth
                    Image{
                        width: 25
                        height: 25
                        anchors.centerIn: parent
                        sourceSize.width: 35
                        sourceSize.height: 35
                        source: model.iconSource
                        Ink{
                            anchors.fill: parent
                            onClicked: sendEmoji(model.iconSource,model.shortName)
                        }
                    }
                }
            }

        }

    }

    View{
        property int activeIndex: 0
        id:emojiesButtonsRowID
        width: parent.width
        height: 50
        x:0
        y:parent.height-50

        ActionButton{
            x:parent.width/10 - width/2
            onClicked: emojiesButtonsRowID.activeIndex = 0
            id:emojiesButton0ID
            width: 30
            height: 30

            Icon{
                scale: 0.8
                anchors.centerIn: parent
                source: "qrc:///Emoji/emoji/category_icons/recent.svg"
            }
            state: if(emojiesButtonsRowID.activeIndex == 0){"EN"}else{"DS"}
            states:[
                State{
                    name: "EN"
                    PropertyChanges{
                        target: emojiesButton0ID
                        backgroundColor: Palette.colors[theme.primaryColor]["300"]
                    }
                },
                State{
                    name:"DS"
                    PropertyChanges{
                        target: emojiesButton0ID
                        backgroundColor: Palette.colors["grey"]["300"]
                    }
                }
            ]


        }

        ActionButton{
            x:parent.width*2/10 - width/2
            onClicked: emojiesButtonsRowID.activeIndex = 1
            id:emojiesButton1ID
            width: 30
            height: 30
            Icon{
                scale: 0.8
                anchors.centerIn: parent
                source: "qrc:///Emoji/emoji/category_icons/people.svg"
            }
            state: if(emojiesButtonsRowID.activeIndex == 1){"EN"}else{"DS"}
            states:[
                State{
                    name: "EN"
                    PropertyChanges{
                        target: emojiesButton1ID
                        backgroundColor: Palette.colors[theme.primaryColor]["300"]
                    }
                },
                State{
                    name:"DS"
                    PropertyChanges{
                        target: emojiesButton1ID
                        backgroundColor: Palette.colors["grey"]["300"]
                    }
                }
            ]

        }

        ActionButton{
            x:parent.width*3/10 - width/2
            onClicked: emojiesButtonsRowID.activeIndex = 2
            id:emojiesButton2ID
            width: 30
            height: 30
            Icon{
                scale: 0.8
                anchors.centerIn: parent
                source: "qrc:///Emoji/emoji/category_icons/foods.svg"
            }
            state: if(emojiesButtonsRowID.activeIndex == 2){"EN"}else{"DS"}
            states:[
                State{
                    name: "EN"
                    PropertyChanges{
                        target: emojiesButton2ID
                        backgroundColor: Palette.colors[theme.primaryColor]["300"]
                    }
                },
                State{
                    name:"DS"
                    PropertyChanges{
                        target: emojiesButton2ID
                        backgroundColor: Palette.colors["grey"]["300"]
                    }
                }
            ]
        }

        ActionButton{
            x:parent.width*4/10 - width/2
            onClicked: emojiesButtonsRowID.activeIndex = 3
            id:emojiesButton3ID
            width: 30
            height: 30
            Icon{
                scale: 0.8
                anchors.centerIn: parent
                source: "qrc:///Emoji/emoji/category_icons/nature.svg"
            }
            state: if(emojiesButtonsRowID.activeIndex == 3){"EN"}else{"DS"}
            states:[
                State{
                    name: "EN"
                    PropertyChanges{
                        target: emojiesButton3ID
                        backgroundColor: Palette.colors[theme.primaryColor]["300"]
                    }
                },
                State{
                    name:"DS"
                    PropertyChanges{
                        target: emojiesButton3ID
                        backgroundColor: Palette.colors["grey"]["300"]
                    }
                }
            ]
        }

        ActionButton{
            x:parent.width*5/10 - width/2
            onClicked: emojiesButtonsRowID.activeIndex = 4
            id:emojiesButton4ID
            width: 30
            height: 30
            Icon{
                scale: 0.8
                anchors.centerIn: parent
                source: "qrc:///Emoji/emoji/category_icons/activity.svg"
            }
            state: if(emojiesButtonsRowID.activeIndex == 4){"EN"}else{"DS"}
            states:[
                State{
                    name: "EN"
                    PropertyChanges{
                        target: emojiesButton4ID
                        backgroundColor: Palette.colors[theme.primaryColor]["300"]
                    }
                },
                State{
                    name:"DS"
                    PropertyChanges{
                        target: emojiesButton4ID
                        backgroundColor: Palette.colors["grey"]["300"]
                    }
                }
            ]
        }

        ActionButton{
            x:parent.width*6/10 - width/2
            onClicked: emojiesButtonsRowID.activeIndex = 5
            id:emojiesButton5ID
            width: 30
            height: 30
            Icon{
                scale: 0.8
                anchors.centerIn: parent
                source: "qrc:///Emoji/emoji/category_icons/objects.svg"
            }
            state: if(emojiesButtonsRowID.activeIndex == 5){"EN"}else{"DS"}
            states:[
                State{
                    name: "EN"
                    PropertyChanges{
                        target: emojiesButton5ID
                        backgroundColor: Palette.colors[theme.primaryColor]["300"]
                    }
                },
                State{
                    name:"DS"
                    PropertyChanges{
                        target: emojiesButton5ID
                        backgroundColor: Palette.colors["grey"]["300"]
                    }
                }
            ]
        }

        ActionButton{
            x:parent.width*7/10 - width/2
            onClicked: emojiesButtonsRowID.activeIndex = 6
            id:emojiesButton6ID
            width: 30
            height: 30
            Icon{
                scale: 0.8
                anchors.centerIn: parent
                source: "qrc:///Emoji/emoji/category_icons/symbols.svg"
            }
            state: if(emojiesButtonsRowID.activeIndex == 6){"EN"}else{"DS"}
            states:[
                State{
                    name: "EN"
                    PropertyChanges{
                        target: emojiesButton6ID
                        backgroundColor: Palette.colors[theme.primaryColor]["300"]
                    }
                },
                State{
                    name:"DS"
                    PropertyChanges{
                        target: emojiesButton6ID
                        backgroundColor: Palette.colors["grey"]["300"]
                    }
                }
            ]
        }

        ActionButton{
            x:parent.width*8/10 - width/2
            onClicked: emojiesButtonsRowID.activeIndex = 7
            id:emojiesButton7ID
            width: 30
            height: 30
            Icon{
                scale: 0.8
                anchors.centerIn: parent
                source: "qrc:///Emoji/emoji/category_icons/travel.svg"
            }
            state: if(emojiesButtonsRowID.activeIndex == 7){"EN"}else{"DS"}
            states:[
                State{
                    name: "EN"
                    PropertyChanges{
                        target: emojiesButton7ID
                        backgroundColor: Palette.colors[theme.primaryColor]["300"]
                    }
                },
                State{
                    name:"DS"
                    PropertyChanges{
                        target: emojiesButton7ID
                        backgroundColor: Palette.colors["grey"]["300"]
                    }
                }
            ]
        }

        ActionButton{
            x:parent.width*9/10 - width/2
            onClicked: emojiesButtonsRowID.activeIndex = 8
            id:emojiesButton8ID
            width: 30
            height: 30
            Icon{
                scale: 0.8
                anchors.centerIn: parent
                source: "qrc:///Emoji/emoji/category_icons/flags.svg"
            }
            state: if(emojiesButtonsRowID.activeIndex == 8){"EN"}else{"DS"}
            states:[
                State{
                    name: "EN"
                    PropertyChanges{
                        target: emojiesButton8ID
                        backgroundColor: Palette.colors[theme.primaryColor]["300"]
                    }
                },
                State{
                    name:"DS"
                    PropertyChanges{
                        target: emojiesButton8ID
                        backgroundColor: Palette.colors["grey"]["300"]
                    }
                }
            ]
        }

    }


}
