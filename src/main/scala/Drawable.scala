import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color

trait Drawable {
  val drawing: Drawing
  var selected: Boolean

  def properties: scalafx.scene.Node
  def draw(graphics: GraphicsContext, select: Boolean): Unit
  def createSaveString: String
  def containsClick(x: Double, y: Double): Boolean
  def move(x: Double, y: Double): Unit
  def moveDistance(x: Double, y: Double): Unit
  def moveTo(x: Double, y: Double): Unit
  def updateColor(color: Color): Unit
  def getColor: Color
  def setFill(fill: Boolean): Unit
  def getFill: Boolean
  def setLineWidth(width: Double): Unit
  def getLineWidth: Double
  def setFontSize(fontSize: Double): Unit
  def getFontSize: Double
  def setWidth(width: Double): Unit
  def getWidth: Double
  def setHeight(height: Double): Unit
  def getHeight: Double
  def getX: Double
  def getY: Double
  def delete(): Unit
  def select(): Unit
}
