import scalafx.scene.control.TreeItem

class URCreateSelected(private val pairs: Map[Drawable, TreeItem[Drawable]]) extends UndoRedo{
  def execute() = {
   for(i <- pairs.keySet) {
      i.selected = false
      val parent = pairs(i)
      parent.children += new TreeItem(i)
      parent.getValue match {
        case group: Group => {
          group.addNode(i)
        }
        case _ =>
      }
    }
    pairs.keySet.toList.head.drawing.draw()
  }

  def getCounter = {
    new URDeleteSelected(pairs)
  }
}
