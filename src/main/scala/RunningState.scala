import scala.util.{Success, Try}

case class RunningState(dataPointer: Int, data: Map[Int, Byte], instructionPointer: Int, output: Seq[Byte], input: InputState[_])(implicit val source: Seq[Command]) {

  def instruction: Option[Command] =
    source.lift(instructionPointer)

  def instruct: Option[Try[RunningState]] =
    instruction.map(_.run(this))

  def shiftDataPointer(amount: Byte): RunningState =
    copy(dataPointer = dataPointer + amount)

  def shiftDataValue(amount: Byte): RunningState =
    copy(data = data.updated(dataPointer, (dataValue + amount).toByte))

  def readInput: RunningState = {
    val (dataValue, nextInput) = input.nextByte
    copy(data = data.updated(dataPointer, dataValue), input = nextInput)
  }

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

  def init(input: InputState[_], source: Seq[Command]): RunningState =
    RunningState(0, Map.empty, 0, Seq.empty, input)(source)

}



