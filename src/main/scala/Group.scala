import Main.drawings
import scalafx.event.ActionEvent
import scalafx.scene.Node
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.control.{Button, ColorPicker, ComboBox, Label, TreeItem}
import scalafx.scene.layout.{HBox, VBox}
import scalafx.Includes._
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.Black
import Constants._
import Style._

import scala.collection.mutable.Buffer

class Group(val drawing: Drawing) extends Drawable {
  private var changeType = Group.Rotate
  private var val1 = 0.0
  private var val2 = 0.0
  private var props: Option[Node] = None
  private var initProps = true
  private var color = Black
  private var fill = false
  private var lineWidth = startLineWidth
  private var fontSize = startFontSize
  private var selectionGroup = false
  var children = List[Drawable]()
  var selected = false

  def makeSelectionGroup() = {
    selectionGroup = true
  }

  def add(drawable: Drawable) = {
    children = children :+ drawable
  }

  def draw(graphics: GraphicsContext, forceSelect: Boolean) = {
    graphics.save()

    if(changeType == Group.Rotate) {
      graphics.rotate(val1)
    } else if(changeType == Group.Scale) {
      graphics.scale(val1, val2)
    } else if(changeType == Group.Translate) {
      graphics.translate(val1, val2)
    } else if(changeType == Group.Shear) {
      graphics.transform(1.0, val1, val2, 1.0, 0.0, 0.0)
    }

    for(i <- children) {
      if((Main.mode != "Select" && this.isSelected && !this.isRoot) || forceSelect) {
        i.draw(graphics, true)
      } else {
        i.draw(graphics, false)
      }

    }

    graphics.restore()
  }

  def isRoot: Boolean = {
    val tab = Main.tabPane.selectionModel().selectedIndex()
    val(_, tree) = drawings(tab)
    tree.getRoot.getValue == this
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
    if(tree.getRoot.getValue == this) {
      tree.selectionModel().select(tree.getRoot)
    } else {
      selectFrom(tree.getRoot)
    }
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
    if(selectionGroup) {
      for(i <- children) {
        i.delete()
      }
      this.children = List[Drawable]()
    } else {
    val tab = Main.tabPane.selectionModel().selectedIndex()
    val(_, tree) = drawings(tab)
        if(tab >= 0) {
          val originalSelect = tree.selectionModel().selectedItem()
          this.select()

          def removeTree(removedFrom: TreeItem[Drawable], drawable: Drawable): Unit = removedFrom.getValue match {
            case helper: Group =>
              helper.removeNode(drawable)
              val newList = removedFrom.children.filter(_.getValue != drawable)
              removedFrom.children = Seq[TreeItem[Drawable]]()
              for(i <- newList) {
                removedFrom.children += i
              }
              drawing.draw()
            case other =>
              removeTree(removedFrom.getParent, this)
          }
          if(tree.getRoot.getValue == this) {
            val childrenList = this.children
            for(i <- this.children) {
              i.delete()
            }
            this.children = List[Drawable]()
          } else {
            removeTree(tree.selectionModel().selectedItem().getParent, this)
            if(originalSelect != null) {
              originalSelect.getValue.select()
            }
          }

        }
    }
        drawing.draw()
  }

  def addNode(drawable: Drawable) = {
    children = children :+ drawable
    initProps = true
  }

  def removeNode(drawable: Drawable) = {
    val first = this.children.takeWhile(_ != drawable)
    var second = this.children.dropWhile(_ != drawable)
    second = second.drop(1)
    val newChildren = first ++ second
    children = newChildren
    initProps = true
  }

  //gets the descendants of this groups excluding other groups
  def getDescendants: Buffer[Drawable] = {
    var descendants = Buffer[Drawable]()
    for(i <- children) {
      i match {
        case group: Group => {
          descendants = descendants ++ group.getDescendants
        }
        case drawable: Drawable => {
          descendants += drawable
        }
      }
    }
    descendants
  }

  def properties: scalafx.scene.Node = {
    if(initProps) {

      val propertyPanel = new VBox

      val colorChange = new ColorPicker(color)
      colorChange.onAction = (event: ActionEvent) => {
        color = colorChange.value()
        var pairs = Map[Drawable, Color]()
        for(i <- this.getDescendants) {
          pairs += (i -> i.getColor)
        }
        drawing.addUndo(new URGroupColor(pairs))

        for(i <- children) {
          i.updateColor(color)
        }
        drawing.draw()
      }

       val fillAll = new Button("Fill all")
       fillAll.onAction = (event: ActionEvent) => {

        var pairs = Map[Drawable, Boolean]()
        for(i <- this.getDescendants) {
          pairs += (i -> i.getFill)
        }
         for(i <- children) {
          i.setFill(true)
        }
        drawing.addUndo(new URGroupFill(pairs))
        drawing.draw()
      }
      val unFillAll = new Button("Unfill all")
      unFillAll.onAction = (event: ActionEvent) => {
        var pairs = Map[Drawable, Boolean]()
        for(i <- this.getDescendants) {
          pairs += (i -> i.getFill)
        }
        for(i <- children) {
          i.setFill(false)
        }
        drawing.addUndo(new URGroupFill(pairs))
        drawing.draw()
      }

      val lineWidthPane = Main.textLabelHelper(lineWidth.toString(), "brush size", a => {
        lineWidth = a.toDouble
        var pairs = Map[Drawable, Double]()
        for(i <- this.getDescendants) {
          pairs += (i -> i.getLineWidth)
        }
        drawing.addUndo(new URGroupLineWidth(pairs))
        for(i <- children) {
          i.setLineWidth(lineWidth)
        }
        drawing.draw()
      })

      val fontSizePane = Main.textLabelHelper(fontSize.toString(), "font size", a => {
        fontSize = a.toDouble
        var pairs = Map[Drawable, Double]()
        for(i <- this.getDescendants) {
          pairs += (i -> i.getFontSize)
        }
        drawing.addUndo(new URGroupFontSize(pairs))
         for(i <- children) {
           i.setFontSize(fontSize)
         }
         drawing.draw()
       })

      val deleteButton = new Button("Delete all")
      deleteButton.onAction = (event: ActionEvent) => {

        val tab = Main.tabPane.selectionModel().selectedIndex()
        val(_, tree) = drawings(tab)
        if(tab >= 0) {
          val selectTree = tree.selectionModel().selectedItem()
          if(tree.getRoot.getValue != this && !this.selectionGroup) {
            drawing.addUndo(new URCreate(this, selectTree.getParent.getValue))
          } else if(this.selectionGroup) {
            var pairs = Map[Drawable, TreeItem[Drawable]]()
            for(i <- this.children) {
              i.select()
              val parent = tree.selectionModel().getSelectedItem.getParent
              pairs += (i -> parent)
            }
            drawing.addUndo(new URCreateSelected(pairs))

          } else {
            drawing.addUndo(new URCreateChildren(this, this.children))
          }
        }
        delete()
        drawing.draw()
      }
      val val1Pane = Main.textLabelHelper(val1.toString(), "x or angle", a => {
        val1 = a.toDouble
        drawing.draw()
      })

      val val2Pane = Main.textLabelHelper(val2.toString(), "y", a => {
        val2 = a.toDouble
        drawing.draw()
      })

      val set = new ComboBox(Group.values.toSeq)
      set.onAction = (event: ActionEvent) => {
        changeType = set.selectionModel().selectedItem()
        drawing.draw()
      }
      set.selectionModel().select(changeType)

      val betaWarning = new Label("WARNING\nTools below are\nin beta testing.\nThey cause bugs.")
      betaWarning.setStyle(betaWarningStyle)

      val betaTools = new VBox
      betaTools.children = List(betaWarning, set, val1Pane, val2Pane)
      val basicTools = new VBox
      val fillTools = new HBox
      fillTools.children = List(fillAll, unFillAll)
      basicTools.children = List(lineWidthPane, fontSizePane, colorChange, fillTools, deleteButton)

      if(selectionGroup) {
        val amountSelect = new Label("Selected: " + children.size)
        amountSelect.setStyle(basicSelectedTextStyle)
        propertyPanel.children += amountSelect
      }

      propertyPanel.children += (basicTools)
      propertyPanel.children += (betaTools)
      propertyPanel.spacing = propertyBoxSpacing
      props = Some(propertyPanel)
    }
    initProps = false

    props.get
  }

  def containsClick(clickX: Double, clickY: Double) = {
    var containsClick = false
    for(i <- children) {
      if(i.containsClick(clickX, clickY)) {
        containsClick = true
      }
    }
    containsClick
  }

  def move(clickX: Double, clickY: Double) = {
    for(i <- children) {
      i.moveDistance(clickX, clickY)
    }
    drawing.draw()
    initProps = true
  }

  def moveDistance(moveX: Double, moveY: Double) = {
    for(i <- children) {
      i.moveDistance(moveX, moveY)
    }
    drawing.draw()
    initProps = true
  }

  def moveTo(moveX: Double, moveY: Double) = {

  }

  def getX = -1

  def getY = -1

  def updateColor(color: Color) = {
    this.color = color
    for(i <- children) {
      i.updateColor(color)
    }
    drawing.draw()
    initProps = true
  }

  def getColor = color

  def setFill(newFill: Boolean) = {
    fill = newFill
    for(i <- children) {
      i.setFill(newFill)
    }
    drawing.draw()
    initProps = true
  }

  def getFill = fill

  def setLineWidth(newWidth: Double) = {
    lineWidth = newWidth
    for(i <- children) {
      i.setLineWidth(newWidth)
    }
    drawing.draw()
    initProps = true
  }

  def getLineWidth = lineWidth

  def setFontSize(fontSize: Double): Unit = {
    this.fontSize = fontSize
    for(i <- children) {
      i.setFontSize(fontSize)
    }
    drawing.draw()
    initProps = true
  }

  def getFontSize = -1

  def setWidth(width: Double): Unit = {
    for(i <- children) {
      i.setWidth(width)
    }
    drawing.draw()
    initProps = true
  }

  def getWidth = -1

  def setHeight(height: Double): Unit = {
    for(i <- children) {
      i.setHeight(height)
    }
    drawing.draw()
    initProps = true
  }

  def getHeight = -1

  //Number of groups children and their children combined
  def numberOfDescendants: Int = {
    var number = 0
    for(i <- children) {
      i match {
        case group: Group => number += (group.numberOfDescendants + 1)
        case _ => number += 1
      }
    }
    number
  }

  def createSaveString: String = {
    var saveString = "group," + this.numberOfDescendants
    for(i <- children) {
      saveString += "\n"
      saveString += i.createSaveString
    }
    saveString
  }

  override def toString: String = {
    if(this.isRoot) {
      "Root Group"
    } else {
      "Group"
    }
  }
}

object Group extends Enumeration {
  val Rotate, Scale, Translate, Shear = Value
}
