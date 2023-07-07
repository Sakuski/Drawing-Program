import scalafx.scene.control.TreeItem

class URDeleteSelected(private val pairs: Map[Drawable, TreeItem[Drawable]]) extends UndoRedo {
  def execute() = {
    for(i <- pairs.keys) {
      i.delete()
    }
  }

  def getCounter = {
    new URCreateSelected(pairs)
  }
}
