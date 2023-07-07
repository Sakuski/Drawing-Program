class URMove(private val drawable: Drawable, private val x: Double, private val y: Double) extends UndoRedo{
  def execute() = {
    drawable.moveDistance(x, y)
  }
  def getCounter: UndoRedo = {
    new URMove(drawable, -1 * x, -1 * y)
  }
}
