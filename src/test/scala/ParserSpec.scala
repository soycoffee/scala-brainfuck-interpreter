import org.scalatest.FunSpec

class ParserSpec extends FunSpec {

  val parser = Parser()

  describe("apply") {

    it("hello_world") {
      val loader = Loader()
      val commands = (loader andThen parser)("hello_world.brainfuck").get
      assert(commands.map(_.syntax).mkString == "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
    }

  }

}
