object Command {
  private val commands = Map[String, (String, Drawing) => Any](
    "help" -> ((args, drawing) => "Available commands:\nundo n -> Calls undo n times.\nredo n -> Calls redo n times."),
    "undo" -> ((args, drawing) => {
      drawing.undo(args.toInt)
      "Undo was called " + args + " times successfully."
    }),
    "redo" -> ((args, drawing) => {
      drawing.redo(args.toInt)
      "Redo was called " + args + " times successfully."
    })
  )

  def command(cmd: String, drawing: Drawing): Any = {
    val argIndex = cmd.indexOf(" ")
    val (name, args) = argIndex match {
      case -1 => (cmd, "")
      case _ => cmd.splitAt(argIndex)
    }

    if(commands.contains(name.toLowerCase())) {
      commands(name.toLowerCase())(args.trim, drawing)
    } else {
      "Please enter a valid command."
    }
  }
}