import Main.{drawings, tabPane}
import scalafx.scene.control.TreeView
import scalafx.Includes._

class URDelete(private val drawable: Drawable) extends UndoRedo{
  def execute() = {
    drawable.delete()
  }

  def getCounter = {

    val tab = tabPane.selectionModel().selectedIndex()

      val (_, tree) = drawings(tab)
      val originalSelect = tree.selectionModel().getSelectedItem.getValue
      drawable.select()
      val parent = tree.selectionModel().getSelectedItem.getParent
      originalSelect.select()
      new URCreate(drawable, parent.getValue)

  }

}
