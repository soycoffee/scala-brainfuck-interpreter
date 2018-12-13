import scala.util.Try

case class Runner(input: Input.Type) extends (Seq[Command] => Try[RunningState]) {

  def apply(source: Seq[Command]): Try[RunningState] =
    source.foldLeft(RunningState.init(source, input).success)((state, command) => state.flatMap(command.run))

}
