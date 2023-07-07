class URGroupFill(private val pairs: Map[Drawable, Boolean]) extends UndoRedo{
  def execute() = {
    for(i <- pairs.keys) {
      i.setFill(pairs(i))
    }
    pairs.keySet.toList.head.drawing.draw()
  }

  def getCounter = {
    var newPairs = Map[Drawable, Boolean]()
    for(i <- pairs.keys) {
      newPairs += (i -> i.getFill)
    }
    new URGroupFill(newPairs)
  }
}
