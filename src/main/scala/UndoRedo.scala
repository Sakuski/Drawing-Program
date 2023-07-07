import scalafx.scene.control.TreeView

trait UndoRedo {
  def execute(): Unit
  def getCounter: UndoRedo
}
