class URText(private val textObject: Text, private val text: String) extends UndoRedo{
  def execute() = {
    textObject.setText(text)
    textObject.select()
  }

  def getCounter = {
    new URText(textObject, textObject.getText)
  }
}
