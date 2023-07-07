import scalafx.scene.control.Label
import scalafx.scene.paint.Color

class URColor(private val drawable: Drawable, private val color: Color) extends UndoRedo{
  def execute() = {
    drawable.updateColor(color)
    drawable.select()
  }
  def getCounter = {
    new URColor(drawable, drawable.getColor)
  }
}
