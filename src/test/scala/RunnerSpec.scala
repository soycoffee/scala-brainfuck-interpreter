import org.scalatest.FunSpec

class RunnerSpec extends FunSpec {

  val parser = Parser()

  describe("apply") {

    it("subtract 2 numbers") {
      val runner = Runner(InputState.NilIn)
      val state = (parser andThen (_.get) andThen runner)("+++++>+++[-<->]<.").get
      assert(state.output.mkString == "4")
    }

    it("multiply 2 numbers") {
      val runner = Runner(InputState.NilIn)
      val state = (parser andThen (_.get) andThen runner)("+++++[->+++<]>.").get
      assert(state.output.mkString == "3")
    }

    it("divide 2 numbers") {
      val runner = Runner(InputState.NilIn)
      val state = (parser andThen (_.get) andThen runner)("++++++++++++[--->+<]>.").get
      assert(state.output.mkString == "1")
    }

  }

}
