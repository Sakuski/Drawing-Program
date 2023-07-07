class URFill(private val drawable: Drawable, private val fill: Boolean) extends UndoRedo{
  def execute() = {
    drawable.setFill(fill)
    drawable.select()
  }

  def getCounter: URFill = {
    new URFill(drawable, !fill)
  }
}
