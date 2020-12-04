package y2015

import scala.util.parsing.combinator.JavaTokenParsers

trait Day12 extends JavaTokenParsers {

  private val Num = """-?\d+""".r

  def countNums(str: String): Int = Num.findAllIn(str).map(_.toInt).sum

  trait Json {
    def count: Int = 0
  }

  case object JsonNull extends Json

  case class JsonBoolean(b: Boolean) extends Json

  case class JsonString(s: String) extends Json

  case class JsonNumber(x: BigDecimal) extends Json {
    override def count: Int = x.toInt
  }

  case class JsonArray(elems: List[Json]) extends Json {
    override def count: Int = elems.map(_.count).sum
  }

  case class JsonObject(entries: List[(String, Json)]) extends Json {
    override def count: Int = {
      if (entries.exists(_._2 == JsonString("red"))) {
        0
      } else {
        (entries.collect {
          case (_, obj) => obj.count
        }).sum
      }
    }
  }

  def trimQuotes(s: String): String = {
    if (s.length < 2) s
    else {
      def f(s: String) = if (s.head == '"') s.tail else s
      def b(s: String) = if (s.last == '"') s.init else s
      f(b(s))
    }
  }

  def expr: Parser[Json] = (
    obj
      | arr
      | stringLiteral ^^ {x => JsonString(trimQuotes(x)) }
      | floatingPointNumber ^^ (s => JsonNumber(BigDecimal(s)))
      | "null" ^^ (_ => JsonNull)
      | "true" ^^ (_ => JsonBoolean(true))
      | "false" ^^ (_ => JsonBoolean(false))
    )

  def obj: Parser[JsonObject] = "{" ~> repsep(member, ",") <~ "}" ^^ JsonObject

  def arr: Parser[JsonArray] = "[" ~> repsep(expr, ",") <~ "]" ^^ JsonArray

  def member: Parser[(String, Json)] = stringLiteral ~ ":" ~ expr ^^ {
    case k ~ ":" ~ v => trimQuotes(k) -> v
  }

  def parseAll(in: CharSequence): ParseResult[Json] = parseAll(expr, in)

  def count(json: String): Int = parseAll(expr, json).get.count
}
