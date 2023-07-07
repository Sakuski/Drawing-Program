class URGroupFontSize(private val pairs: Map[Drawable, Double]) extends UndoRedo{
  def execute() = {
    for(i <- pairs.keys) {
      i.setFontSize(pairs(i))
    }
    pairs.keySet.toList.head.drawing.draw()
  }

  def getCounter = {
    var newPairs = Map[Drawable, Double]()
    for(i <- pairs.keys) {
      newPairs += (i -> i.getFontSize)
    }
    new URGroupFontSize(newPairs)
  }
}