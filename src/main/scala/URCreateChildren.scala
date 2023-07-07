import Main.tabPane
import Main.drawings
import scalafx.Includes._
import scalafx.scene.control.TreeItem

class URCreateChildren(private val group: Group, private val children: List[Drawable]) extends UndoRedo {
  def execute() = {
    group.select()

    val tab = tabPane.selectionModel().selectedIndex()
    if(tab >= 0) {
      val (drawing, tree) = drawings(tab)

      def addTo(to: TreeItem[Drawable], drawable: Drawable): Unit = to.getValue match{
            case g: Group =>
              g.addNode(drawable)
              to.children += new TreeItem(drawable)
              drawing.draw()
            case other =>
              addTo(to.getParent, drawable)
      }

      def attachChildren(to: TreeItem[Drawable], list: List[Drawable]): Unit =  {

            for(j <- list) {
              j match{
                case group: Group => {
                  addTo(tree.selectionModel().selectedItem(), group)
                  group.select()
                  val curSel = tree.selectionModel().selectedItem()
                  val kids = group.children
                  group.children = List[Drawable]()
                  attachChildren(curSel, kids)
              }
              case other: Drawable => {
                addTo(tree.selectionModel().selectedItem(), other)
              }
            }
        }
      }
      this.group.select()
      attachChildren(tree.selectionModel().selectedItem(), this.children)
    }
  }

  def getCounter = {
    new URDeleteChildren(group)
  }
}
