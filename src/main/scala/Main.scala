import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.layout.{Background, BackgroundFill, CornerRadii, VBox}
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color._
import scalafx.Includes._
import scalafx.event.{ActionEvent, Event}
import scalafx.geometry.{Insets, Orientation}
import scalafx.scene.control.MenuBar
import scalafx.scene.layout.BorderPane
import scalafx.scene.control._
import scalafx.scene.input.MouseEvent
import scalafx.stage.FileChooser
import Constants._
import Style._

import java.io.PrintWriter
import scala.io.Source
import scala.collection.mutable.Buffer

object Main extends JFXApp{
  var drawings = Buffer[(Drawing, TreeView[Drawable])]()
  var mode = startMode
  private var color = Black
  private var shape = startShape
  private var lineWidth = startLineWidth
  private var fill = startFill
  private var fontSize = startFontSize
  private var selected: Option[Group] = None

  stage = new JFXApp.PrimaryStage {
    title.value = "Drawing Program"
    width = startWidth
    height = startHeight
  }
  val menuBar = new MenuBar
  val root = new BorderPane
  val tabPane = new TabPane
  val leftBox = new VBox
  leftBox.spacing = leftBoxSpacing

  //mode selector
  val modeBox = new VBox
  val modeText = new Label("Current mode:")
  modeText.setStyle(basicLeftBoxTextStyle)
  modeBox.background = new Background(Array(new BackgroundFill((Gray), CornerRadii.Empty, Insets.Empty)))
  val selectedMode = new Label(mode)
  selectedMode.setStyle(leftBoxBoldStyle)
  val modeMenu = new MenuButton("Select mode")
  val drawMode = new MenuItem("Draw")
  val editMode = new MenuItem("Edit")
  val textMode = new MenuItem("Text")
  val selectMode = new MenuItem("Select")
  modeMenu.items = List(drawMode, editMode, textMode, selectMode)
  modeBox.children.add(modeText)
  modeBox.children.add(selectedMode)
  modeBox.children.add(modeMenu)


  drawMode.onAction = (event: ActionEvent) => {
    mode = "Draw"
    selectedMode.text = mode
    update()
  }
  editMode.onAction = (event: ActionEvent) => {
    mode = "Edit"
    selectedMode.text = mode
    update()
  }
  textMode.onAction = (event: ActionEvent) => {
    mode = "Text"
    selectedMode.text = mode
    update()
  }
  selectMode.onAction = (event: ActionEvent) => {
    mode = "Select"
    selectedMode.text = mode
    update()
  }
  leftBox.children.add(modeBox)

  //color picker
  val colorBox = new VBox
  val colorText = new Label("Choose color")
  colorText.setStyle(basicLeftBoxTextStyle)
  colorBox.background = new Background(Array(new BackgroundFill((Gray), CornerRadii.Empty, Insets.Empty)))
  val colorPicker = new ColorPicker(color)
  colorBox.children.add(colorText)
  colorBox.children.add(colorPicker)
  leftBox.children.add(colorBox)

  colorPicker.onAction = (event: ActionEvent) => {
      color = colorPicker.value()
  }

  //shape selector
  val shapeBox = new VBox
  val shapeText = new Label("Current shape:")
  shapeText.setStyle(basicLeftBoxTextStyle)
  val selectedShape = new Label(shape)
  selectedShape.setStyle(leftBoxBoldStyle)
  shapeBox.background = new Background(Array(new BackgroundFill((Gray), CornerRadii.Empty, Insets.Empty)))
  val shapeMenu = new MenuButton("Select shape")
  val rectangle = new MenuItem("Rectangle")
  val ellipse = new MenuItem("Ellipse")
  val line = new MenuItem("Line")
  val circle = new MenuItem("Circle")
  shapeMenu.items = List(line, rectangle, ellipse, circle)
  shapeBox.children.add(shapeText)
  shapeBox.children.add(selectedShape)
  shapeBox.children.add(shapeMenu)

  rectangle.onAction = (event: ActionEvent) => {
    shape = "Rectangle"
    selectedShape.text = shape
  }
  ellipse.onAction = (event: ActionEvent) => {
    shape = "Ellipse"
    selectedShape.text = shape
  }
  line.onAction = (event: ActionEvent) => {
    shape = "Line"
    selectedShape.text = shape
  }
  circle.onAction = (event: ActionEvent) => {
    shape = "Circle"
    selectedShape.text = shape
  }
  val fillBox = new CheckBox("Fill")
  fillBox.setStyle(basicLeftBoxTextStyle)
  fillBox.onAction = (event: ActionEvent) => {
    fill = !fill
  }
  shapeBox.children.add(fillBox)
  leftBox.children.add(shapeBox)

  //brush selector
  val brushBox = new VBox
  brushBox.background = new Background(Array(new BackgroundFill((Gray), CornerRadii.Empty, Insets.Empty)))
  val brushSizeSelector = this.textLabelHelper(lineWidth.toString(), "", a => {
    lineWidth = a.toDouble
  })
  val brushText = new Label("Brush size:")
  brushText.setStyle(basicLeftBoxTextStyle)
  brushBox.children.add(brushText)
  brushBox.children.add(brushSizeSelector)
  shapeBox.children.add(brushBox)

  //Text
  val textBox = new VBox
  val textText = new Label("Font size:")
  textText.setStyle(basicLeftBoxTextStyle)
  textBox.background = new Background(Array(new BackgroundFill((Gray), CornerRadii.Empty, Insets.Empty)))
  val fontSizeSelector = this.textLabelHelper(fontSize.toString(), "", a => {
    fontSize = a.toDouble
  })
  textBox.children.add(textText)
  textBox.children.add(fontSizeSelector)
  leftBox.children.add(textBox)

  //Group
  val groupBox = new VBox
  groupBox.background = new Background(Array(new BackgroundFill((Gray), CornerRadii.Empty, Insets.Empty)))
  val addGroup = new Button("Add Group")

  addGroup.onAction = (event: ActionEvent) => {
    val tab = tabPane.selectionModel().selectedIndex()
      val (drawing, tree) = drawings(tab)
      if(tab >= 0) {
        val (currentDrawing, treeView) = drawings(tab)
        val drawable: Drawable = new Group(drawing)
        val selectTree = treeView.selectionModel().selectedItem()
        currentDrawing.temp = None

        def addTree(toBeAdded: TreeItem[Drawable]): Unit = toBeAdded.getValue match{
          case helper: Group =>
            helper.addNode(drawable)
            toBeAdded.children += new TreeItem(drawable)
            currentDrawing.draw()
          case other =>
            addTree(toBeAdded.getParent)
        }

        if(selectTree != null) {
          addTree(selectTree)
        }
        drawing.addUndo(new URDelete(drawable))
      }
      drawing.draw()
  }
  groupBox.children.add(addGroup)
  leftBox.children.add(groupBox)

  val startDrawing: Drawing = new Drawing
  val (tab, tree) = makeTab("Untitled", startDrawing)
  drawings = drawings :+ startDrawing -> tree

  tabPane += tab
  root.top = menuBar
  root.center = tabPane
  root.left = leftBox


  val scene = new Scene(root)
  stage.scene = scene

  //Setting up top bar menus
  val menuFile = new Menu("File")
  menuBar.getMenus.add(menuFile)
  val exit = new MenuItem("Exit")
  val newDrawing = new MenuItem("New")
  val open = new MenuItem("Open")
  val save = new MenuItem("Save")
  menuFile.items = List(newDrawing, open, save, exit)

  val menuEdit = new Menu("Edit")
  menuBar.getMenus.add(menuEdit)
  val undo = new MenuItem("Undo")
  val redo = new MenuItem("Redo")
  menuEdit.items = List(undo, redo)

  exit.onAction = (event: ActionEvent) => {
    sys.exit(0)
  }

  newDrawing.onAction = (event: ActionEvent) => {
    if(tabPane.tabs.size() == 0) {
      drawings = drawings.empty
    }
    val drawing = new Drawing
    val (drawingTab, tree) = makeTab("Untitled", drawing)
    drawings = drawings :+ drawing -> tree
    tabPane += drawingTab
  }

  save.onAction = (event: ActionEvent) => {
    val selected = tabPane.selectionModel().selectedIndex()
    if(selected >= 0) {
      val (selDrawing, _) = drawings(selected)
      val fileChooser = new FileChooser {
        title = "Save Drawing"
      }
      val file = fileChooser.showSaveDialog(stage)
      if(file != null) {
        if(file.getName.endsWith(".txt")) {
          try {
            val writer = new PrintWriter(file)
            writer.write(selDrawing.createSaveString)
            writer.close()
            tabPane.tabs(selected).text = file.getName
          }
        }
      }
    }
  }

  open.onAction = (event: ActionEvent) => {
    if(tabPane.tabs.size() == 0) {
      drawings = drawings.empty
    }
    val fileChooser = new FileChooser {
      title = "Open Drawing"
    }
    val file = fileChooser.showOpenDialog(stage)
    if(file != null) {
      if(file.getName.endsWith(".txt")) {
        try {
          val lines = Source.fromFile(file).getLines().toList
          val drawing = new Drawing
          val (drawingTab, tree) = makeTab(file.getName, drawing)
          drawings = drawings :+ drawing -> tree
          tabPane += drawingTab
          tabPane.selectionModel().select(drawingTab)
          drawing.initOpenedFile(lines, tree)
        }
      }
    }
  }

  undo.onAction = (event: ActionEvent) => {
    val tab = Main.tabPane.selectionModel().selectedIndex()
    if(tab >= 0) {
      val(drawing, tree) = drawings(tab)
      update()
      drawing.undo(1)
    }
  }

  redo.onAction = (event: ActionEvent) => {
    val tab = Main.tabPane.selectionModel().selectedIndex()
    if(tab >= 0) {
      val(drawing, tree) = drawings(tab)
      update()
      drawing.redo(1)
    }

  }

  leftBox.background = new Background(Array(new BackgroundFill((Gray), CornerRadii.Empty, Insets.Empty)))

  //make drawing
  private def makeTab(name: String, drawing: Drawing): (Tab, TreeView[Drawable]) = {
    val drawTree: TreeView[Drawable] = new TreeView[Drawable]
    drawTree.root = drawing.drawingTree()
    val drawScroll = new ScrollPane
    drawScroll.content = drawTree
    val properties = new ScrollPane
    drawTree.prefWidth <== drawScroll.width
    drawTree.prefHeight <== drawScroll.height

    val right = new SplitPane
    val top = new SplitPane
    val left = new SplitPane
    val tab = new Tab

    val leftTopBorder = new BorderPane
    val leftBottomBorder = new BorderPane
    val canvas = new Canvas(canvasWidth, canvasHeight)
    val graphics = canvas.graphicsContext2D
    drawing.setGraphicsContext(graphics)
    val canvasAdjuster = new ScrollPane
    canvasAdjuster.content = canvas
    leftTopBorder.center = canvasAdjuster

    val commandPane = new TextField
    val commandDisplay = new TextArea
    commandPane.background = new Background(Array(new BackgroundFill((Black), CornerRadii.Empty, Insets.Empty)))
    commandPane.setStyle(consoleInputStyle)
    commandDisplay.background = new Background(Array(new BackgroundFill((Black), CornerRadii.Empty, Insets.Empty)))
    commandDisplay.setStyle(consoleOutputStyle)
    commandDisplay.editable = false
    leftBottomBorder.top = commandPane
    leftBottomBorder.center = commandDisplay

    top.items ++= List(left, right)
    top.dividerPositions = startTopDividerPos

    left.orientation = Orientation.Vertical
    left.items ++= List(leftTopBorder, leftBottomBorder)
    left.dividerPositions = startLeftDividerPos

    right.items ++= List(drawScroll, properties)
    right.orientation = Orientation.Vertical

    // Command handling
    commandPane.onAction = (event: ActionEvent) => {
      commandDisplay.appendText("> " + commandPane.text() + "\n" + Command.command(commandPane.text(), drawing) + "\n")
      commandPane.text = ""
    }

    // Drawing
    var anchorX = 0.0
    var anchorY = 0.0
    var xDistance = 0.0
    var yDistance = 0.0
    canvas.onMousePressed = (event: MouseEvent) => {
      anchorX = event.x
      anchorY = event.y
      xDistance = 0.0
      yDistance = 0.0

      def selectFrom(from: TreeItem[Drawable]): Unit = {
        for(i <- from.getChildren) {
          if(i.getValue.containsClick(anchorX, anchorY)) {
            i.getValue match {
              case group: Group =>  {
                selectFrom(i)
              }
              case drawable: Drawable => {
                drawTree.selectionModel().select(i)
              }
            }
          }
        }
      }
      if(mode == "Edit") {
        selectFrom(drawTree.getRoot)
      } else if(mode == "Select"){
        selectFrom(drawTree.getRoot)
        val item = drawTree.selectionModel().getSelectedItem.getValue
        val group = selected.get
        if(group.children.contains(item)) {
          if(item.containsClick(anchorX, anchorY)) {
            item.selected = false
            group.removeNode(item)
            properties.content = group.properties
            drawing.draw()
          }
        } else {
          if(item.containsClick(anchorX, anchorY)) {
            item.selected = true
            group.addNode(item)
            properties.content = group.properties
            drawing.draw()
          }
        }
      }else if(mode == "Text") {

        var text = ""
        val dialog = new TextInputDialog("")
        dialog.setHeaderText("Write your text here")
        dialog.setTitle("Add text")
        dialog.setContentText("Text: ")
        val addedText = dialog.showAndWait()

        for(i <- addedText) {
          text = i
        }
        val tab = tabPane.selectionModel().selectedIndex()
        val (drawing, tree) = drawings(tab)
        if(tab >= 0) {
          val (currentDrawing, treeView) = drawings(tab)
          val drawable: Drawable = new Text(anchorX, anchorY, text, fontSize, color, drawing)
          val selectTree = treeView.selectionModel().selectedItem()
          currentDrawing.temp = None

          def addTree(toBeAdded: TreeItem[Drawable]): Unit = toBeAdded.getValue match{
            case helper: Group =>
              helper.addNode(drawable)
              toBeAdded.children += new TreeItem(drawable)
              currentDrawing.draw()
            case other =>
              addTree(toBeAdded.getParent)
          }

          if(selectTree != null && text != "") {
            addTree(selectTree)
            drawing.addUndo(new URDelete(drawable))
          }

       }
        drawing.draw()
      }
    }
    canvas.onMouseDragged = (event: MouseEvent) => {
      if(mode == "Draw") {
      drawing.temp = shape match {
        case "Line" => Some(new Line(anchorX, anchorY, event.x, event.y, lineWidth, color, drawing))
        case "Rectangle" => Some(new Rectangle(Math.min(anchorX, event.x), Math.min(anchorY, event.y), Math.abs(anchorX-event.x), Math.abs(anchorY - event.y), color, lineWidth, fill, drawing))
        case "Ellipse" => Some(new Ellipse(Math.min(anchorX, event.x), Math.min(anchorY, event.y), Math.abs(anchorX-event.x), Math.abs(anchorY - event.y), color, lineWidth, fill, drawing))
        case "Circle" => Some(new Ellipse(anchorX, anchorY, Math.max(Math.abs(anchorX-event.x), Math.abs(anchorY - event.y)), Math.max(Math.abs(anchorX-event.x), Math.abs(anchorY - event.y)), color, lineWidth, fill, drawing))
      }
      } else if(mode == "Edit") {
        val edited = drawTree.selectionModel().selectedItem().getValue
        xDistance += (event.x-anchorX)
        yDistance += (event.y-anchorY)
        edited.moveDistance(event.x - anchorX, event.y - anchorY)
        anchorX = event.x
        anchorY = event.y
        drawTree.selectionModel().selectedItem().setValue(edited)
        val currentValue = drawTree.selectionModel().selectedItem()
        if(currentValue != null) {

          properties.content = currentValue.getValue.properties
        } else {
          properties.content = new Label("Nothing has been selected")
        }
      } else if(mode == "Select") {
        val group = selected.get
        xDistance += (event.x-anchorX)
        yDistance += (event.y-anchorY)
        group.move(event.x - anchorX, event.y - anchorY)
        anchorX = event.x
        anchorY = event.y
      }
      drawing.draw()
    }
    canvas.onMouseReleased = (event: MouseEvent) => {
      if(mode == "Draw") {
      val tab = tabPane.selectionModel().selectedIndex()
      val (drawing, tree) = drawings(tab)
      if(tab >= 0) {
        val (currentDrawing, treeView) = drawings(tab)
        val drawable: Drawable = shape match {
          case "Line" => new Line(anchorX, anchorY, event.x, event.y, lineWidth, color, drawing)
          case "Rectangle" => new Rectangle(Math.min(anchorX, event.x), Math.min(anchorY, event.y), Math.abs(anchorX-event.x), Math.abs(anchorY - event.y), color, lineWidth, fill, drawing)
          case "Ellipse" => new Ellipse(Math.min(anchorX, event.x), Math.min(anchorY, event.y), Math.abs(anchorX-event.x), Math.abs(anchorY - event.y), color, lineWidth, fill, drawing)
          case "Circle" => new Ellipse(anchorX, anchorY, Math.max(Math.abs(anchorX-event.x), Math.abs(anchorY - event.y)), Math.max(Math.abs(anchorX-event.x), Math.abs(anchorY - event.y)), color, lineWidth, fill, drawing)
        }
        val selectTree = treeView.selectionModel().selectedItem()
        currentDrawing.temp = None

        def addTree(toBeAdded: TreeItem[Drawable]): Unit = toBeAdded.getValue match{
          case helper: Group =>
            helper.addNode(drawable)
            toBeAdded.children += new TreeItem(drawable)
            currentDrawing.draw()
          case other =>
            addTree(toBeAdded.getParent)
        }

        if(selectTree != null) {
          addTree(selectTree)
        }
        drawing.addUndo(new URDelete(drawable))
      }

    } else if(mode == "Edit") {
        val edited = drawTree.selectionModel().selectedItem().getValue
        drawing.addUndo(new URMove(edited, -1*xDistance, -1*yDistance))
      } else if(mode == "Select") {
        drawing.addUndo(new URMove(selected.get, -1*xDistance, -1*yDistance))
      }
      drawing.draw()
    }

    drawTree.selectionModel().selectedItem.onChange {
      val currentValue = drawTree.selectionModel().selectedItem()
      if(currentValue != null && mode != "Select") {

        properties.content = currentValue.getValue.properties
      } else if(mode == "Select") {
        properties.content = selected.get.properties
      }else {
        properties.content = new Label("Nothing has been selected")
      }
      drawing.draw()
    }
    drawTree.selectionModel().selectFirst()

    tab.onCloseRequest = (event: Event) => {
      val index = tabPane.selectionModel().getSelectedIndex
      drawings.remove(index)
    }

    tab.text = name
    tab.content = top
    tab -> drawTree

  }

  //help to make Drawable's propertydisplays
  def textLabelHelper(text: String, label: String, action: String => Unit): BorderPane = {
    val propBorderPane = new BorderPane
    propBorderPane.left = new Label(label)
    val propText = new TextField
    propText.text = text
    propBorderPane.center = propText

    propText.onAction = (event: ActionEvent) => {
      action(propText.text())
    }

    propText.focused.onChange(if(!propText.focused()) action(propText.text()))

    propBorderPane
  }

  //Forces the gui to update
  def update() = {
   val tab = tabPane.selectionModel().selectedIndex()
    if(tab >= 0) {
      val (drawing, tree) = drawings(tab)
      drawing.draw()
      for(i <-selected) {
        for(i <- i.children) {
          i.selected = false
        }
      }
      selected = Some(new Group(drawing))
      selected.get.makeSelectionGroup()
      if(tree.selectionModel().getSelectedItem != null) {
      val ogSelect = tree.selectionModel().getSelectedItem.getValue
      if(ogSelect != null) {
        if(ogSelect == tree.getRoot.getValue) {
          for(i <- tree.getRoot.getChildren) {
            i.getValue.select()
          }
        } else {
          tree.getRoot.getValue.select()
        }
        ogSelect.select()
      } else {
        tree.getRoot.getValue.select()
      }
      }
    }
  }
}
