import Main.tabPane
import Main.drawings
import scalafx.scene.control.TreeItem
import scalafx.Includes._

class URCreate(private val drawable: Drawable, private val parent: Drawable) extends UndoRedo{
  def execute() = {
    parent.select()

    val tab = tabPane.selectionModel().selectedIndex()
    if(tab >= 0) {
      val (drawing, tree) = drawings(tab)
      val selectedTree = tree.selectionModel().selectedItem()
      def addTo(to: TreeItem[Drawable], drawable: Drawable): Unit = to.getValue match{
            case helper: Group =>
              helper.addNode(drawable)
              to.children += new TreeItem(drawable)
              drawing.draw()
            case other =>
              addTo(to.getParent, drawable)
      }
      if(selectedTree != null) {
        addTo(selectedTree, this.drawable)
      }

      this.drawable match {
        case group: Group => {

          def attachChildren(to: TreeItem[Drawable], list: List[Drawable]): Unit =  {

            for(i <- list) {
              i match{
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
          this.drawable.select()
          val kids = group.children
          group.children = List[Drawable]()
          attachChildren(tree.selectionModel().selectedItem(), kids)
        }

        case _ =>
      }

      drawing.draw()
    }
    drawable.select()
  }
  def getCounter = {
    new URDelete(drawable)
  }
}
