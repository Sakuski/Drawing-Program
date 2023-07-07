class URMoveLineTo(private val line: Line, private val x1: Double, private val y1: Double, private val x2: Double, private val y2: Double) extends UndoRedo {
  def execute() = {
    line.moveLineTo(x1, y1, x2, y2)
    line.select()
  }

  def getCounter = {
    new URMoveLineTo(line, line.x1, line.y1, line.x2, line.y2)
  }
}
