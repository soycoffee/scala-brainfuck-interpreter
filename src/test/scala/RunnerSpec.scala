import org.scalatest.FunSpec

class RunnerSpec extends FunSpec {

  val parser = Parser()

  describe("apply") {

    it("subtract 2 numbers") {
      val runner = Runner(InputState.NilIn)
      val state = (parser andThen (_.get) andThen runner)("+++++>+++[-<->]<.").get
      assert(state.output.mkString == "2")
    }

    it("multiply 2 numbers") {
      val runner = Runner(InputState.NilIn)
      val state = (parser andThen (_.get) andThen runner)("+++++[->+++<]>.").get
      assert(state.output.mkString == "15")
    }

    it("divide 2 numbers") {
      val runner = Runner(InputState.NilIn)
      val state = (parser andThen (_.get) andThen runner)("++++++++++++[--->+<]>.").get
      assert(state.output.mkString == "4")
    }

  }

}
