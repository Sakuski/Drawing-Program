import Main.drawings
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.control.{Button, CheckBox, ColorPicker, Label, TextField, TreeItem}
import scalafx.scene.Node
import scalafx.scene.layout.{BorderPane, VBox}
import scalafx.scene.paint.Color
import scalafx.Includes._
import scalafx.event.ActionEvent
import Constants._

class Rectangle(private var x: Double, private var y: Double, private var width: Double, private var height: Double, private var color: Color, var lineWidth: Double, var fill: Boolean, val drawing: Drawing) extends Drawable{
  private var props: Option[Node] = None
  private var initProps = true
  var selected = false

  def draw(graphics: GraphicsContext, forceSelect: Boolean) = {
    if(selected || (this.isSelected && Main.mode != "Select") || forceSelect) {
      graphics.fill = Color.LightBlue
      graphics.fillRect(x-selectOverExtension, y-selectOverExtension, width + 2*selectOverExtension, height + 2*selectOverExtension)
    }
    if(fill) {
      graphics.fill = color
      graphics.fillRect(x, y, width, height)
    } else {
      graphics.setStroke(color)
      graphics.setLineWidth(lineWidth)
      graphics.strokeRect(x, y, width, height)
    }
  }
  def select() = {
    val tab = Main.tabPane.selectionModel().selectedIndex()
    val(_, tree) = drawings(tab)

    def selectFrom(from: TreeItem[Drawable]): Unit = {
        for(i <- from.getChildren) {
          if(i.getValue == this) {
            tree.selectionModel().select(i)
            } else {
            i.getValue match {
              case group: Group => {
                selectFrom(i)
              }
              case _ =>
            }
          }
        }
      }
    selectFrom(tree.getRoot)
    drawing.draw()
  }
  def isSelected: Boolean = {
    val tab = Main.tabPane.selectionModel().selectedIndex()
    val(_, tree) = drawings(tab)
    if(tree.selectionModel().getSelectedItem == null) {
      false
    } else {
      if(tree.selectionModel().getSelectedItem.getValue == this) {
        true
      } else {false}
    }
  }

  def delete() = {
    val tab = Main.tabPane.selectionModel().selectedIndex()
    val(_, tree) = drawings(tab)
        if(tab >= 0) {
          val originalSelect = tree.selectionModel().selectedItem()
          this.select()

          def removeTree(removedFrom: TreeItem[Drawable]): Unit = removedFrom.getValue match {
            case helper: Group =>
              helper.removeNode(this)
              val newList = removedFrom.children.filter(_.getValue != this)
              removedFrom.children = Seq[TreeItem[Drawable]]()
              for(i <- newList) {
                removedFrom.children += i
              }
              drawing.draw()
            case other =>
              removeTree(removedFrom.getParent)
          }
          removeTree(tree.selectionModel().selectedItem().getParent)
          if(originalSelect != null) {
            originalSelect.getValue.select()
          }
        }
        drawing.draw()
  }

  def properties: scalafx.scene.Node = {
    if(initProps) {

      val propertyPanel = new VBox

      val colorChange = new ColorPicker(color)
      colorChange.onAction = (event: ActionEvent) => {
        val oldColor = color
        color = colorChange.value()
        if(oldColor != color) {
          drawing.addUndo(new URColor(this, oldColor))
        }
        drawing.draw()
      }

      val xPane = Main.textLabelHelper(x.toString(), "x", a => {
        val oldX = x
        x = a.toDouble
        if(oldX != x) {
          drawing.addUndo(new URMoveTo(this, oldX, y))
        }
        drawing.draw()
      })

      val yPane = Main.textLabelHelper(y.toString(), "y", a => {
        val oldY = y
        y = a.toDouble
        if(oldY != y) {
          drawing.addUndo(new URMoveTo(this, x, oldY))
        }
        drawing.draw()
      })

      val widthPane = Main.textLabelHelper(width.toString(), "width", a => {
        val oldWidth = width
        width = a.toDouble
        if(oldWidth != width) {
          drawing.addUndo(new URWidth(this, oldWidth))
        }
        drawing.draw()
      })

      val heightPane = Main.textLabelHelper(height.toString(), "height", a => {
        val oldHeight = height
        height = a.toDouble
        if(oldHeight != height) {
          drawing.addUndo(new URHeight(this, oldHeight))
        }
        drawing.draw()
      })

      val lineWidthPane = Main.textLabelHelper(lineWidth.toString(), "brush size", a => {
        val oldLineWidth = lineWidth
        lineWidth = a.toDouble
        if(oldLineWidth != lineWidth) {
          drawing.addUndo(new URLineWidth(this, oldLineWidth))
        }
        drawing.draw()
      })

      val fillCheck = new CheckBox("Fill")
      if(fill) {
        fillCheck.selected = true
      } else {
        fillCheck.selected = false
      }
      fillCheck.onAction = (event: ActionEvent) => {
        val oldFill = fill
        fill = !fill
        drawing.addUndo(new URFill(this, oldFill))
        drawing.draw()
      }

      val deleteButton = new Button("Delete")
      deleteButton.onAction = (event: ActionEvent) => {

        val tab = Main.tabPane.selectionModel().selectedIndex()
        val(_, tree) = drawings(tab)

        if(tab >= 0) {
          val selectTree = tree.selectionModel().selectedItem()
          drawing.root.removeNode(this)
          drawing.addUndo(new URCreate(this, selectTree.getParent.getValue))

          def removeTree(removedFrom: TreeItem[Drawable]): Unit = removedFrom.getValue match {
            case helper: Group =>
              helper.removeNode(this)
              val newList = removedFrom.children.filter(_.getValue != this)
              removedFrom.children = Seq[TreeItem[Drawable]]()
              for(i <- newList) {
                removedFrom.children += i
              }
              drawing.draw()
            case other =>
              removeTree(removedFrom.getParent)
          }
          if(selectTree != null) {
            removeTree(selectTree)
          }
        }
        drawing.draw()
      }

      propertyPanel.children = List(xPane, yPane, widthPane, heightPane, lineWidthPane, colorChange, fillCheck, deleteButton)
      props = Some(propertyPanel)
      initProps = false
    }

    props.get
  }

  def containsClick(clickX: Double, clickY: Double): Boolean = {
    (clickX >= x && clickX <= x + width && clickY >= y && clickY <= y + height)
  }

  def move(clickX: Double, clickY: Double): Unit = {
    x = clickX - (width/2)
    y = clickY - (height/2)
    initProps = true
  }

  def moveDistance(moveX: Double, moveY: Double) = {
    x += moveX
    y += moveY
    drawing.draw()
    initProps = true
  }

  def moveTo(moveX: Double, moveY: Double) = {
    x = moveX
    y = moveY
    initProps = true
  }

  def getX = x

  def getY = y

  def updateColor(color: Color) = {
    this.color = color
    initProps = true
    drawing.draw()
  }

  def getColor = color

  def setFill(newFill: Boolean) = {
    fill = newFill
    initProps = true
  }

  def getFill = fill

  def setLineWidth(newWidth: Double) = {
    lineWidth = newWidth
    initProps = true
  }

  def getLineWidth = lineWidth

  def setFontSize(fontSize: Double): Unit = {

  }

  def getFontSize = -1

  def setWidth(width: Double): Unit = {
    this.width = width
    drawing.draw()
    initProps = true
  }

  def getWidth = width

  def setHeight(height: Double): Unit = {
    this.height = height
    drawing.draw()
    initProps = true
  }

  def getHeight = height

    def createSaveString = {
      val fillString = fill match {
        case true => 1.toString
        case false => 0.toString
      }
      val colorString = "" + color.red + "_" + color.green + "_" + color.blue + "_" + color.opacity
      "rectangle," + x + "," + y + "," + width + "," + height + "," + colorString + "," + lineWidth + "," + fillString
    }

   override def toString: String = {
      "Rectangle"
   }

}
