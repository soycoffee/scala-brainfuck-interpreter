import scala.util.{Success, Try}

case class RunningState(dataPointer: Int, data: Map[Int, Byte], instructionPointer: Int, output: Seq[Byte])(implicit val source: Seq[Command], val input: Input.Type) {

  def shiftDataPointer(amount: Byte): RunningState =
    copy(dataPointer = dataPointer + amount)

  def shiftDataValue(amount: Byte): RunningState =
    copy(data = data.updated(dataPointer, (dataValue + amount).toByte))

  def readInput: RunningState =
    copy(data = data.updated(dataPointer, input()))

  def pushOutput: RunningState =
    copy(output = output :+ dataValue)

  def dataValue: Byte =
    data.getOrElse(dataPointer, 0)

  def next: RunningState =
    jump(instructionPointer + 1)

  def jump(destination: Int): RunningState =
    copy(instructionPointer = destination)

  def success: Try[RunningState] =
    Success(this)

}

object RunningState {

  def init(implicit source: Seq[Command], input: Input.Type) =
    RunningState(0, Map.empty, 0, Seq.empty)

}



