class URHeight(private val drawable: Drawable, private val height: Double) extends UndoRedo {
  def execute() = {
    drawable.setHeight(height)
    drawable.select()
  }

  def getCounter = {
    new URHeight(drawable, drawable.getHeight)
  }
}
