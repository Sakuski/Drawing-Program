import Main.drawings
import scalafx.scene.Node
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.control.{Button, ColorPicker, TreeItem}
import scalafx.scene.paint.Color
import scalafx.Includes._
import scalafx.event.ActionEvent
import scalafx.scene.layout.VBox
import Constants._

class Line(var x1: Double, var y1: Double, var x2: Double, var y2: Double, var width: Double, var color: Color, val drawing: Drawing) extends Drawable{
  private var props: Option[Node] = None
  private var initProps = true
  var selected = false

  def draw(graphics: GraphicsContext, forceSelect: Boolean) = {
    if(selected || (Main.mode != "Select" && this.isSelected) || forceSelect) {
      graphics.setLineWidth(width + 2*selectOverExtension)
      graphics.setStroke(Color.LightBlue)
      graphics.strokeLine(x1, y1, x2, y2)
    }

    graphics.setLineWidth(width)
    graphics.setStroke(color)
    graphics.strokeLine(x1, y1, x2, y2)
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
        if(color != oldColor) {
          drawing.addUndo(new URColor(this, oldColor))
        }
        drawing.draw()
      }

      val x1Pane = Main.textLabelHelper(x1.toString(), "startX", a => {
        val oldX1 = x1
        x1 = a.toDouble
        if(oldX1 != x1) {
          drawing.addUndo(new URMoveLineTo(this, oldX1, y1, x2, y2))
        }
        drawing.draw()
      })

      val y1Pane = Main.textLabelHelper(y1.toString(), "startY", a => {
        val oldY1 = y1
        y1 = a.toDouble
        if(oldY1 != y1) {
          drawing.addUndo(new URMoveLineTo(this, x1, oldY1, x2, y2))
        }
        drawing.draw()
      })

      val x2Pane = Main.textLabelHelper(x2.toString(), "endX", a => {
        val oldX2 = x2
        x2 = a.toDouble
        if(oldX2 != x2) {
          drawing.addUndo(new URMoveLineTo(this, x1, y1, oldX2, y2))
        }
        drawing.draw()
      })

      val y2Pane = Main.textLabelHelper(y2.toString(), "endY", a => {
        val oldY2 = y2
        y2 = a.toDouble
        if(oldY2 != y2) {
          drawing.addUndo(new URMoveLineTo(this, x1, y1, x2, oldY2))
        }
        drawing.draw()
      })

      val widthPane = Main.textLabelHelper(width.toString(), "width", a => {
        val oldWidth = width
        width = a.toDouble
        if(oldWidth != width) {
          drawing.addUndo(new URLineWidth(this, oldWidth))
        }
        drawing.draw()
      })

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

      propertyPanel.children = List(x1Pane, y1Pane, x2Pane, y2Pane, widthPane, colorChange, deleteButton)
      props = Some(propertyPanel)
    }
    initProps = false
    props.get
  }

  def containsClick(clickX: Double, clickY: Double) = {
    val line = new scalafx.scene.shape.Line()
    val clickError = math.max(10.0, width)
    line.setStartX(x1)
    line.setStartY(y1)
    line.setEndX(x2)
    line.setEndY(y2)
    line.setStrokeWidth(clickError)
    line.contains(clickX, clickY)
  }

  def move(clickX: Double, clickY: Double) = {
    val oldX1 = x1
    val oldY1 = y1
    val oldX2 = x2
    val oldY2 = y2

    x1 = clickX - ((oldX1 - oldX2)/2)
    x2 = clickX + ((oldX1 - oldX2)/2)
    y1 = clickY - ((oldY1 - oldY2)/2)
    y2 = clickY + ((oldY1 - oldY2)/2)
    initProps = true
  }

  def moveDistance(moveX: Double, moveY: Double) = {
    x1 += moveX
    x2 += moveX
    y1 += moveY
    y2 += moveY
    drawing.draw()
    initProps = true
  }

  def moveTo(moveX: Double, moveY: Double) = {

  }

  def getX = -1

  def getY = -1

  def moveLineTo(x1: Double, y1: Double, x2: Double, y2: Double) = {
    this.x1 = x1
    this.y1 = y1
    this.x2 = x2
    this.y2 = y2
    initProps = true
  }

  def updateColor(color: Color) = {
    this.color = color
    initProps = true
  }

  def getColor = color

  def setFill(newFill: Boolean) = {

  }

  def getFill = false

  def setLineWidth(newWidth: Double) = {
    width = newWidth
    initProps = true
  }

  def getLineWidth = width

  def setFontSize(fontSize: Double): Unit = {

  }

  def getFontSize = -1

  def setWidth(width: Double): Unit = {

  }

  def getWidth = -1

  def setHeight(height: Double): Unit = {

  }

  def getHeight = -1

  def createSaveString = {
    val colorString = "" + color.red + "_" + color.green + "_" + color.blue + "_" + color.opacity
    "line," + x1 + "," + y1 + "," + x2 + "," + y2 + "," + colorString + "," + width
  }

  override def toString: String = "Line"
}
