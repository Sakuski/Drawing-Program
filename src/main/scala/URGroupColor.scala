import scalafx.scene.paint.Color

class URGroupColor(private val pairs: Map[Drawable, Color]) extends UndoRedo{
  def execute() = {
    for(i <- pairs.keys) {
      i.updateColor(pairs(i))
    }
    pairs.keySet.toList.head.drawing.draw()
  }

  def getCounter = {
    var newPairs = Map[Drawable, Color]()
    for(i <- pairs.keys) {
      newPairs += (i -> i.getColor)
    }
    new URGroupColor(newPairs)
  }
}
