import Main.drawings
import scalafx.event.ActionEvent
import scalafx.scene.Node
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.control.{Button, ColorPicker, Label, TreeItem}
import scalafx.scene.layout.VBox
import scalafx.scene.paint.Color
import scalafx.Includes._
import scalafx.scene.text.Font

class Text(private var x: Double, private var y: Double, private var text: String, private var fontSize: Double, private var color: Color, val drawing: Drawing) extends Drawable {
   private var props: Option[Node] = None
   private var initProps = true
   var selected = false

   def draw(graphics: GraphicsContext, forceSelect: Boolean) = {
     if(selected || (Main.mode != "Select" && this.isSelected) || forceSelect) {
       graphics.fill = Color.LightBlue
       graphics.fillRect(x, y - fontSize, (text.size*fontSize)/2, y-(y-fontSize))
    }

     graphics.fill = color
     graphics.setFont(Font.font(fontSize))
     graphics.fillText(text, x, y)
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
      new Label("Text field")

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

      val textPane = Main.textLabelHelper(text, "text", a => {
        val oldText = text
        text = a
        if(oldText != text) {
          drawing.addUndo(new URText(this, oldText))
        }
        drawing.draw()
      })

       val fontSizePane = Main.textLabelHelper(fontSize.toString(), "font size", a => {
         val oldFontSize = fontSize
         fontSize = a.toDouble
         if(oldFontSize != fontSize) {
           drawing.addUndo(new URFontSize(this, oldFontSize))
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

      propertyPanel.children = List(xPane, yPane, textPane, fontSizePane, colorChange, deleteButton)
      props = Some(propertyPanel)
     }
     initProps = false
     props.get
   }

  def containsClick(clickX: Double, clickY: Double) = {
    (clickX >= x && clickX <= x + ((text.size*fontSize)/2) && clickY <= y && clickY >= y - fontSize)
  }

  def move(clickX: Double, clickY: Double) = {
    x = clickX
    y = clickY
    initProps = true
  }

  def moveDistance(moveX: Double, moveY: Double) = {
    x += moveX
    y += moveY
    drawing.draw()
    initProps = true
  }

  def moveTo(moveX: Double, moveY: Double) = {
    this.move(moveX, moveY)
  }

  def getX = x

  def getY = y

  def updateColor(color: Color) = {
    this.color = color
    initProps = true
  }

  def getColor = color

  def setFill(newFill: Boolean) = {

  }

  def getFill = false

  def setLineWidth(newWidth: Double) = {

  }

  def getLineWidth = -1

  def setFontSize(fontSize: Double): Unit = {
    this.fontSize = fontSize
    initProps = true
  }

  def getFontSize = this.fontSize

  def setText(text: String): Unit = {
    this.text = text
    initProps = true
  }

  def getText = {
    this.text
  }

  def setWidth(width: Double): Unit = {

  }

  def getWidth = -1

  def setHeight(height: Double): Unit = {

  }

  def getHeight = -1

   def createSaveString: String = {
     val colorString = "" + color.red + "_" + color.green + "_" + color.blue + "_" + color.opacity
     "text," + x + "," + y + "," + text + "," + fontSize + "," + colorString
   }

   override def toString: String = "Text"
}
