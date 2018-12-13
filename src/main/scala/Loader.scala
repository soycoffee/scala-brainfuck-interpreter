case class Loader() extends (String => String) {

  def apply(filename: String): String =
    io.Source.fromFile(filename, "utf-8").getLines().mkString

}
