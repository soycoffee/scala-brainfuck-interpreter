trait InputState[S] {

  def nextByte: (Byte, InputState[S])

}

object InputState {

  case object StdIn extends InputState[Unit] {

    override def nextByte: (Byte, InputState[Unit]) =
      (io.StdIn.readByte, this)

  }

  case class SeqIn(values: Seq[Byte]) extends InputState[Seq[Byte]] {

    override def nextByte: (Byte, InputState[Seq[Byte]]) =
      (values.head, SeqIn(values.tail))

  }

  val NilIn = SeqIn(Nil)

}