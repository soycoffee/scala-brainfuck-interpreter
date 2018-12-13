object Main {

  val input: Input.Type = Input.StdIn

  lazy val loader = Loader()
  lazy val parser = Parser()
  lazy val runner = Runner(input)

  def main(args: Array[String]): Unit = {
    assert(args.length >= 1)
    val result = (loader andThen parser andThen (_.flatMap(runner)))(args(0))
    println(result.get)
  }

}
