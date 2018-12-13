object Input {

  type Type = () => Byte

  val StdIn: Type = io.StdIn.readByte

}