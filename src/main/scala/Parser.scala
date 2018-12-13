import scala.util.{Success, Try}

case class Parser() extends (String => Try[Seq[Command]]) {

  def apply(source: String): Try[Seq[Command]] = {
    (toCommands _ andThen excludeCommentLoop)(source)
  }

  private def toCommands(source: String): Seq[Command] = {
    val commandMap = (Command.Values.map(_.syntax) zip Command.Values).toMap
    source.collect(commandMap)
  }

  private def excludeCommentLoop(commands: Seq[Command]): Try[Seq[Command]] =
    if (commands.headOption.contains(Command.LoopOpen))
      for {
        loopClosePointer <- Command.Loop.findBalanceClose(0, commands)
      } yield commands.drop(loopClosePointer + 1)
    else
      Success(commands)

}
