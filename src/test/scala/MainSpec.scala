import org.scalatest.FunSpec

class MainSpec extends FunSpec {

  val parser = Parser()

  private class MyOutput extends (String => Unit) {

    var last: Option[String] = None

    override def apply(v1: String): Unit =
      last = Some(v1)

  }

  private def buildMain(inputValues: Seq[Byte], myOutput: MyOutput): Main =
    new Main {
      override val input: InputState[_] = InputState.SeqIn(inputValues)
      override val output: String => Unit = myOutput
    }

  describe("apply") {

    it("hello_world") {
      val myOutput = new MyOutput
      val main = buildMain(Seq(), myOutput)
      main.main(Array("hello_world.brainfuck"))
      assert(myOutput.last.get == "Hello World!\n")
    }

  }

}
