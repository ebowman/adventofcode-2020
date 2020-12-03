package y2015

import scala.util.parsing.combinator.RegexParsers

trait Day09 extends RegexParsers {

  import scala.collection.mutable
  val places = new mutable.HashSet[String]()
  val paths = new mutable.HashMap[String, mutable.HashMap[String, Int]]()
  // Faerun to Norrath = 144
  def place: Parser[String] = """[a-zA-Z]+""".r
  def distance: Parser[Int] = """\d+""".r ^^ { _.toInt }
  def edge: Parser[Unit] = place ~ "to" ~ place ~ "=" ~ distance ^^ {
    case from ~ _ ~ to ~ _ ~ dist =>
      places.add(from)
      places.add(to)
      paths.get(from) match {
        case None =>
          val map = new mutable.HashMap[String, Int]
          map.put(to, dist)
          paths.put(from, map)
        case Some(map) =>
          map.put(to, dist)
      }
      paths.get(to) match {
        case None =>
          val map = new mutable.HashMap[String, Int]()
          map.put(from, dist)
          paths.put(to, map)
        case Some(map) =>
          map.put(from, dist)
      }
  }
}
