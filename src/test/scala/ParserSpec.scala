import org.scalatest.FunSpec

class ParserSpec extends FunSpec {

  val loader = Loader()
  val parser = Parser()

  describe("apply") {

    it("hello_world") {
      val commands = (loader andThen parser)("hello_world.brainfuck").get
      assert(commands.map(_.syntax).mkString == "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
    }

  }

}
