import scalafx.scene.control.{TreeItem, TreeView}
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import scalafx.Includes._
import Constants._

import scala.collection.mutable.Buffer

class Drawing {
  var root = new Group(this)
  var temp = None: Option[Drawable]
  private var graphics = None: Option[GraphicsContext]
  private val undoRedo = new UndoRedoManager

  def setGraphicsContext(graphicsContext: GraphicsContext) = {
    graphics = Some(graphicsContext)
    this.draw()
  }

  //A tree structure containing drawables
  def drawingTree(): TreeItem[Drawable] = {
    def helpDraw(drawable: Drawable): TreeItem[Drawable] = drawable match {
      case helper: Group =>
        val helperItem = new TreeItem(drawable)
        for(i <- helper.children) {
          helperItem.children += helpDraw(i)
        }
        helperItem
      case _ => new TreeItem(drawable)
    }
    helpDraw(root)
  }

  def draw(): Unit = {
    for(i <- graphics) {
      i.fill = Color.White
      i.fillRect(0, 0, canvasWidth, canvasHeight)
      root.draw(i, false)
      for(j <- temp) {
        j.draw(i, false)
      }
    }
  }

  def undo(amount: Int) = {
    undoRedo.undo(amount)
  }
  def addUndo(addedUR: UndoRedo) = {
    undoRedo.addUndo(addedUR)
  }

  def redo(amount: Int) = {
    undoRedo.redo(amount)
  }

  def addRedo(addedUR: UndoRedo) = {
    undoRedo.addRedo(addedUR)
  }

  //Creates a save string that is written into a file
  def createSaveString: String = {
    fileKeyString + "\n" + root.createSaveString
  }

  //Creates a drawing from a text files's lines
  def initOpenedFile(lines: List[String], tree: TreeView[Drawable]) = {
    val key = lines.head
    var pieces = lines.drop(2)

    def addTree(toBeAdded: TreeItem[Drawable], drawable: Drawable) = toBeAdded.getValue match{
          case helper: Group =>
            helper.addNode(drawable)
            toBeAdded.children += new TreeItem(drawable)
            this.draw()
          case other =>
            toBeAdded.getParent
    }

    //Helper method to add a items to a group
    def handleGroup(treeItem: TreeItem[Drawable], addPieces: Buffer[String]): Unit = {
      var index = 0
      while(index < addPieces.size) {
        val piece = addPieces(index)
        if(piece.contains("group")) {
          val childrenNumber = piece.split(",")(1).toInt
          val group = Buffer[String]()
          var groupIndex = 1
          while(groupIndex <= childrenNumber) {
            group += addPieces(index + groupIndex)
            groupIndex += 1
          }
          val toAdd = new Group(this)
          addTree(treeItem, toAdd)
          toAdd.select()
          handleGroup(tree.selectionModel().getSelectedItem, group)
          index += childrenNumber
        } else {
          val props = piece.split(",")
          val shape = props(0) match {
            case "line" => {
              val colorParts = props(5).split("_")
              val color = Color(colorParts(0).toDouble, colorParts(1).toDouble, colorParts(2).toDouble, colorParts(3).toDouble)
              new Line(props(1).toDouble, props(2).toDouble, props(3).toDouble, props(4).toDouble, props(6).toDouble, color, this)
            }
            case "rectangle" => {
              val colorParts = props(5).split("_")
              val color = Color(colorParts(0).toDouble, colorParts(1).toDouble, colorParts(2).toDouble, colorParts(3).toDouble)
              val fill = props(7) match {
                case "1" => true
                case "0" => false
              }
              new Rectangle(props(1).toDouble, props(2).toDouble, props(3).toDouble, props(4).toDouble, color, props(6).toDouble, fill, this)
            }
            case "ellipse" => {
              val colorParts = props(5).split("_")
              val color = Color(colorParts(0).toDouble, colorParts(1).toDouble, colorParts(2).toDouble, colorParts(3).toDouble)
              val fill = props(7) match {
                case "1" => true
                case "0" => false
              }
              new Ellipse(props(1).toDouble, props(2).toDouble, props(3).toDouble, props(4).toDouble, color, props(6).toDouble, fill, this)
            }
            case "text" => {
              val colorParts = props(5).split("_")
              val color = Color(colorParts(0).toDouble, colorParts(1).toDouble, colorParts(2).toDouble, colorParts(3).toDouble)
              new Text(props(1).toDouble, props(2).toDouble, props(3), props(4).toDouble, color, this)
            }
          }
          addTree(treeItem, shape)
      }
        index += 1
      }
    }

    tree.selectionModel().select(tree.getRoot)
    if(key == fileKeyString) {
      handleGroup(tree.selectionModel().getSelectedItem, pieces.toBuffer)
    }
    this.draw()
    }


  }

