object Main {

  def main(args: Array[String]): Unit = main(args, InputState.StdIn, print)

  def main(args: Array[String], input: InputState[_], output: String => Unit): Unit = {
    assert(args.length >= 1)
    val loader = Loader()
    val parser = Parser()
    val runner = Runner(input)
    val result = (loader andThen parser andThen (_.flatMap(runner)))(args(0))
    output(new String(result.get.output.toArray))
  }

}
