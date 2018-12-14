import scala.util.{Success, Try}

case class Runner(input: InputState[_]) extends (Seq[Command] => Try[RunningState]) {

  def apply(source: Seq[Command]): Try[RunningState] =
    chainStates(source)
        .reduceLeft((a, b) => a flatMap (_ => b))

  private def chainStates(source: Seq[Command]): Stream[Try[RunningState]] =
    chainStates(Success(RunningState.init(input, source)))

  private def chainStates(state: Try[RunningState]): Stream[Try[RunningState]] =
    Stream.cons(state, state match {
      case Success(_state) => _state.instruct match {
        case Some(nextState) => chainStates(nextState)
        case None => Stream.empty
      }
      case failure => Stream(failure)
    })

}
