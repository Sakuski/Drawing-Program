class URFontSize(private val drawable: Drawable, private val fontSize: Double) extends UndoRedo {
  def execute() = {
    drawable.setFontSize(fontSize)
    drawable.select()
  }

  def getCounter = {
    new URFontSize(drawable, drawable.getFontSize)
  }
}
