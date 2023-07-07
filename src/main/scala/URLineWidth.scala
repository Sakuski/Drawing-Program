class URLineWidth(private val drawable: Drawable, private val lineWidth: Double) extends UndoRedo {
  def execute() = {
    drawable.setLineWidth(lineWidth)
    drawable.select()
  }

  def getCounter = {
    new URLineWidth(drawable, drawable.getLineWidth)
  }
}
