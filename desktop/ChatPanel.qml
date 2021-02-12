import QtQuick 2.7

Row{
    anchors.fill: parent
    ChatList{}
    ChatDetail{visible: if(parent.width > 600){true}else{false}}
}
