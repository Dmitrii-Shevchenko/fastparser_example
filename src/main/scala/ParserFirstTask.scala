import scala.io.Source

object ParserFirstTask extends App {
  import fastparse._, NoWhitespace._

  val lines = Source.fromResource("table.txt").getLines().mkString("\n")

  def element[_: P] = P(CharIn("0-9", "A-Z", ".", "\\-", ":", " ", "(", ")", "%", "+", "#", "a-z", "/").rep(1))
  def lineSeparator[_: P] = P("\n")

  def garbageLine[_: P] = P(element ~ lineSeparator)
  def header[_: P] = P(Start ~ garbageLine.rep)
  def footer[_: P] = P(garbageLine.rep ~ End)

  def line[_: P] = P(element.!.rep(min = 2, sep = "\t") ~ lineSeparator.?)
  def table[_: P] =
    P(header.? ~ line.rep ~ footer.?)

  parse(lines, table(_)) match {
    case Parsed.Success(value, successIndex) =>
      println("Success value=" + value + " successIndex=" + successIndex)
    case f @ Parsed.Failure(_) =>
      println("Failure " + f.trace(true))
  }
}
