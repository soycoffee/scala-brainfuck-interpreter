import scala.util.Try

case class Runner(input: InputState[_]) extends (Seq[Command] => Try[RunningState]) {

  def apply(source: Seq[Command]): Try[RunningState] =
    source.foldLeft(RunningState.init(input, source).success)((state, command) => state.flatMap(command.run))

}
