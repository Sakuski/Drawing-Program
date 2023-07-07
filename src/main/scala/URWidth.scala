class URWidth(private val drawable: Drawable, private val width: Double) extends UndoRedo{
  def execute() = {
    drawable.setWidth(width)
    drawable.select()
  }

  def getCounter = {
    new URWidth(drawable, drawable.getWidth)
  }
}
