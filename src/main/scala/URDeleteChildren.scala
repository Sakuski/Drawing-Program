class URDeleteChildren(private val parent: Group) extends UndoRedo {
  def execute() = {
    for(i <- parent.children) {
      i.delete()
    }
  }

  def getCounter: UndoRedo = {
    new URCreateChildren(parent, parent.children)
  }
}
