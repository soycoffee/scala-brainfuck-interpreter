trait Main {

  def input: InputState[_]
  def output: String => Unit

  lazy val loader = Loader()
  lazy val parser = Parser()
  lazy val runner = Runner(input)

  def main(args: Array[String]): Unit = {
    assert(args.length >= 1)
    val result = (loader andThen parser andThen (_.flatMap(runner)))(args(0))
    output(new String(result.get.output.toArray))
  }

}

object Main extends Main {

  override def input: InputState[_] = InputState.StdIn
  override def output: String => Unit = print

}
