class URMoveTo(private val drawable: Drawable, private val x: Double, private val y: Double) extends UndoRedo {
  def execute() = {
    drawable.moveTo(x, y)
    drawable.select()
  }

  def getCounter = {
    new URMoveTo(drawable, drawable.getX, drawable.getY)
  }
}
