import scala.Function.tupled
import scala.util.{Failure, Success, Try}

sealed abstract class Command(val syntax: Char)(behaviour: Command.Behaviour) {

  def run(state: RunningState): Try[RunningState] =
    behaviour(state)

}

object Command {

  case object DataPointerIncrement extends Command('>')(_.shiftDataPointer(+1).next.success)
  case object DataPointerDecrement extends Command('<')(_.shiftDataPointer(-1).next.success)
  case object DataValueIncrement extends Command('+')(_.shiftDataValue(+1).next.success)
  case object DataValueDecrement extends Command('-')(_.shiftDataValue(-1).next.success)
  case object DataValueOutput extends Command('.')(_.pushOutput.next.success)
  case object DataValueInput extends Command(',')(_.readInput.next.success)
  case object LoopOpen extends Command('[')(Loop.Behaviour(_.dataValue == 0)(Loop.findBalanceClose))
  case object LoopClose extends Command(']')(Loop.Behaviour(_.dataValue != 0)(Loop.findBalanceOpen))

  val Values = Seq(
    DataPointerIncrement,
    DataPointerDecrement,
    DataValueIncrement,
    DataValueDecrement,
    DataValueOutput,
    DataValueInput,
    LoopOpen,
    LoopClose,
  )

  type Behaviour = RunningState => Try[RunningState]

  object Loop {

    private[Command] def Behaviour(cond: RunningState => Boolean)(findBalance: RunningState => Try[Int]): Behaviour =
      { state =>
        if (cond(state))
          for {
            loopBalancePointer <- findBalance(state)
          } yield
            state.jump(loopBalancePointer + 1)
        else
          state.next.success
      }

    def findBalanceOpen(state: RunningState): Try[Int] =
      findBalanceOpen(state.instructionPointer, state.source)

    def findBalanceOpen(instructionPointer: Int, source: Seq[Command]): Try[Int] =
      findBalance(LoopOpen, Range(instructionPointer, -1, -1), source)

    def findBalanceClose(state: RunningState): Try[Int] =
      findBalanceClose(state.instructionPointer, state.source)

    def findBalanceClose(instructionPointer: Int, source: Seq[Command]): Try[Int] =
      findBalance(LoopClose, Range(instructionPointer, source.length, +1), source)

    private def findBalance(needle: Command, instructionPointers: Range, source: Seq[Command]): Try[Int] = {
      val nestCounts = calculateNestCounts(instructionPointers, source)
      (instructionPointers zip nestCounts)
        .find(tupled ((i, nestCount) => source(i) == needle && nestCount == 0))
        .map(_._1)
        .fold[Try[Int]](Failure(BalanceNotFound(instructionPointers)))(Success(_))
    }

    private def calculateNestCounts(instructionPointers: Seq[Int], source: Seq[Command]): Seq[Int] = {
      var currentCount = 0
      instructionPointers
        .map({ i =>
          source(i) match {
            case LoopOpen => currentCount += 1
            case LoopClose => currentCount -= 1
            case _ =>
          }
          currentCount
        })
    }

    case class BalanceNotFound(range: Range) extends Exception(s"range=$range")

  }

}
