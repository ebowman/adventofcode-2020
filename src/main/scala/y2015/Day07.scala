package y2015

import scala.util.parsing.combinator.RegexParsers

trait Day07 extends RegexParsers {

  import scala.collection.mutable

  val wires = new mutable.HashMap[String, Signal]

  trait Signal {
    def name: String

    def voltage: Int
  }

  case class Named(name: String) extends Signal {
    override def voltage: Int = {
      val v = wires(name).voltage
      wires.put(name, Wire(name, v)) // memoizing is SUPER important for performance!
      v
    }
  }

  case class Wire(name: String, override val voltage: Int) extends Signal

  case class OpAnd(name: String, s1: Signal, s2: Signal) extends Signal {
    override def voltage: Int = (s1.voltage & s2.voltage)
  }

  case class OpOr(name: String, s1: Signal, s2: Signal) extends Signal {
    override def voltage: Int = (s1.voltage | s2.voltage)
  }

  case class OpLShift(name: String, s: Signal, shift: Int) extends Signal {
    override def voltage: Int = (s.voltage << shift) & 0xFFFF
  }

  case class OpRShift(name: String, s: Signal, shift: Int) extends Signal {
    override def voltage: Int = s.voltage >> shift
  }

  case class OpNot(name: String, s: Signal) extends Signal {
    override def voltage: Int = (~s.voltage) & 0xFFFF
  }

  def num: Parser[Int] = """\d+""".r ^^ { _.toInt }

  def name: Parser[String] = """[a-z]+""".r ^^ { x => x }

  def wire: Parser[Unit] = num ~ "->" ~ name ^^ { case num ~ _ ~ name => wires.put(name, Wire(name, num)) }

  def wire1: Parser[Unit] = name ~ "->" ~ name ^^ { case s ~ _ ~ name => wires.put(name, Named(s)) }

  def and: Parser[Unit] = name ~ "AND" ~ name ~ "->" ~ name ^^ {
    case s1 ~ _ ~ s2 ~ _ ~ d => wires.put(d, OpAnd(d, Named(s1), Named(s2)))
  }

  def and1: Parser[Unit] = num ~ "AND" ~ name ~ "->" ~ name ^^ {
    case n ~ _ ~ s ~ _ ~ d => wires.put(d, OpAnd(d, Wire("", n), Named(s)))
  }

  def and2: Parser[Unit] = name ~ "AND" ~ num ~ "->" ~ name ^^ {
    case s ~ _ ~ n ~ _ ~ d => wires.put(d, OpAnd(d, Wire("", n), Named(s)))
  }

  def or: Parser[Unit] = name ~ "OR" ~ name ~ "->" ~ name ^^ {
    case s1 ~ _ ~ s2 ~ _ ~ d => wires.put(d, OpOr(d, Named(s1), Named(s2)))
  }

  def or1: Parser[Unit] = num ~ "OR" ~ name ~ "->" ~ name ^^ {
    case n ~ _ ~ s ~ _ ~ d => wires.put(d, OpOr(d, Wire("", n), Named(s)))
  }

  def or2: Parser[Unit] = name ~ "OR" ~ num ~ "->" ~ name ^^ {
    case s ~ _ ~ n ~ _ ~ d => wires.put(d, OpOr(d, Wire("", n), Named(s)))
  }

  def lshift: Parser[Unit] = name ~ "LSHIFT" ~ num ~ "->" ~ name ^^ {
    case s ~ _ ~ shift ~ _ ~ d => wires.put(d, OpLShift(d, Named(s), shift))
  }

  def rshift: Parser[Unit] = name ~ "RSHIFT" ~ num ~ "->" ~ name ^^ {
    case s ~ _ ~ shift ~ _ ~ d => wires.put(d, OpRShift(d, Named(s), shift))
  }

  def _not: Parser[Unit] = "NOT" ~ name ~ "->" ~ name ^^ {
    case _ ~ s ~ _ ~ d => wires.put(d, OpNot(d, Named(s)))
  }

  def parser: Parser[Unit] = wire | wire1 | and | and1 | and2 | or | or1 | or2 | lshift | rshift | _not
}