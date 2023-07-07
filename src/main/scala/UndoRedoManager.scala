import scalafx.scene.control.TreeView

import scala.collection.mutable.Buffer

class UndoRedoManager() {
  private var undoList = Buffer[UndoRedo]()
  private var redoList = Buffer[UndoRedo]()

  def undo(amount: Int) = {
    val handled = Math.min(amount, undoList.size)
    val cmdList = undoList.takeRight(handled).reverse
    undoList = undoList.dropRight(handled)
    for(i <- cmdList) {
      addRedo(i.getCounter)
      i.execute()
    }
  }

  def addUndo(undo: UndoRedo) = {
    undoList += undo
  }

  def redo(amount: Int) = {
    val handled = Math.min(amount, redoList.size)
    val cmdList = redoList.takeRight(handled).reverse
    redoList = redoList.dropRight(handled)
    for(i <- cmdList) {
      addUndo(i.getCounter)
      i.execute()
    }
  }

  def addRedo(redo: UndoRedo) = {
    redoList += redo
  }
}
