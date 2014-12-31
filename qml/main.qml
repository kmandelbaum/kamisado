import QtQuick 2.3
import QtQuick.Controls 1.2

Row {
    Rectangle {
        width: 500
        height: 500
        Rectangle {
            id: root
            transform: Rotation { angle : 180; origin.x : root.width / 2; origin.y : root.height / 2 }
            property int cellSizeY: (0 | parent.height / fieldSizeY )
            property int cellSizeX: (0 | parent.height / fieldSizeY )
            width: cellSizeX * fieldSizeX
            height: cellSizeY * fieldSizeY
            border.color: "black"
            color: "black"
            onWidthChanged: {
                cellSizeChanged( Math.min( width / fieldSizeX, height / fieldSizeY)) + border.width / 2
            }
            onHeightChanged: {
                cellSizeChanged( Math.min( width / fieldSizeX, height / fieldSizeY)) + border.width / 2
            }
            Repeater {
                anchors.fill: parent
                id: field
                model: fieldModel
                Rectangle {
                    width: root.cellSizeX
                    height: root.cellSizeY
                    x: modelData.x * width
                    y: modelData.y * height
                    color: modelData.color
                    opacity: 0.5
                    border.color: "black"
                    border.width: 2
                }
            }
            
            Repeater {
                anchors.fill: parent
                id: pieces
                model: piecesModel
                Canvas {
                    height: root.cellSizeX
                    width: root.cellSizeY
                    x: modelData.x
                    y: modelData.y
                    onPaint: {
                        var ctx = getContext( "2d" )
                        ctx.save( )

                        ctx.beginPath( )
                        ctx.fillStyle = modelData.fillColor
                        ctx.strokeStyle = modelData.lineColor

                        var cx = width / 2
                        var cy = height / 2

                        var r = Math.min( cx, cy) * 0.75
                        ctx.lineWidth = r * 0.25

                        ctx.moveTo( r + cx , 0 + cy)
                        
                        for ( var i = 1; i < 7; ++i ) {
                           var newxx = cx + r * Math.cos( Math.PI * i / 3) 
                           var newyy = cy + r * Math.sin( Math.PI * i / 3) 
                           ctx.lineTo( newxx, newyy)
                        }
                        ctx.fill() 
                        ctx.stroke()
                        ctx.restore()
                    }
                }
            }

            MouseArea  {
                anchors.fill: parent
                acceptedButtons: Qt.AllButtons

                onPositionChanged: {
                    mouseMove( mouse.x, mouse.y, mouse.buttons)
                }
                onPressed: {
                    mouseDown( mouse.x, mouse.y, mouse.buttons)
                }
                onReleased: {
                    mouseUp( mouse.x, mouse.y, mouse.buttons)
                }

            }
        }
    }
    Button {
        text: "New game"
        onClicked: { newGame( ) }
    }
}
