import org.scalatest.FunSpec

class MainSpec extends FunSpec {

  val parser = Parser()

  private class MyOutput extends (String => Unit) {

    var last: Option[String] = None

    override def apply(v1: String): Unit =
      last = Some(v1)

  }

  describe("apply") {

    it("hello_world") {
      val myOutput = new MyOutput
      Main.main(Array("hello_world.brainfuck"), InputState.NilIn, myOutput)
      assert(myOutput.last.get == "Hello World!\n")
    }

    it("rot13") {
      val allLatinAlphabets = (0x41 to 0x5a) ++ (0x61 to 0x7a)
      val eof = -0x01
      val inputValues = (allLatinAlphabets :+ eof).map(_.toByte)
      val myOutput = new MyOutput
      Main.main(Array("rot13.brainfuck"), InputState.SeqIn(inputValues), myOutput)
      assert(myOutput.last.get == "NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm")
    }

  }

}
