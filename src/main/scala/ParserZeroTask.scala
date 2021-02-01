object ParserZeroTask extends App {
  import fastparse._
  import NoWhitespace._

  val lines =
    "header\n73084,96.2276284822943,6.50816898204104,9.34748250169338,70978.2678395575\nfooter"

  def element[_: P] = P(CharIn("0-9", ".", "a-z").rep(1))
  def lineSeparator[_: P] = P("\n")

  def line[_: P] = P(element.!.rep(min = 2, sep = ",") ~ lineSeparator.?)

//  Inconvenient case:
//  def line[_: P] = P((element.rep(1) ~ ",".?).rep(1) ~ lineSeparator.?).!

  def garbageLine[_: P] = P(element ~ lineSeparator)
  def header[_: P] = P(Start ~ garbageLine)
  def footer[_: P] = P(garbageLine ~ End)

  def lineWithoutGarbage[_: P] =
    P(header.? ~ line ~ footer.?)

  parse(lines, lineWithoutGarbage(_)) match {
    case Parsed.Success(value, successIndex) =>
      println("Success value=" + value + " successIndex=" + successIndex)
    case f @ Parsed.Failure(_) =>
      println("Failure " + f.trace(true))
  }
}
