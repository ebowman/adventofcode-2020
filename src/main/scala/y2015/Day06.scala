package y2015

import scala.util.parsing.combinator.RegexParsers

object Lights {

  case class Rect(left: Int, top: Int, width: Int, height: Int)

  object Rect {
    def mkRect(x1: Int, y1: Int, x2: Int, y2: Int): Rect = Rect(x1, y1, x2 - x1 + 1, y2 - y1 + 1)
  }

  class BitGrid(width: Int, height: Int) {
    val grid = Array.ofDim[Boolean](height, width)
    def countTurnedOn(): Int = grid.map(_.count(_ == true)).sum
  }

  trait Command {
    def range: Rect
    def operate(grid: BitGrid): Unit = {
      for (y <- range.top until range.top + range.height) {
        for (x <- range.left until range.left + range.width) {
          write(x, grid.grid(y))
        }
      }
    }

    def write(index: Int, array: Array[Boolean]): Unit
  }

  case class TurnOn(range: Rect) extends Command {
    override def write(index: Int, array: Array[Boolean]): Unit = {
      array(index) = true
    }
  }

  case class TurnOff(range: Rect) extends Command {
    override def write(index: Int, array: Array[Boolean]): Unit = {
      array(index) = false
    }
  }

  case class Toggle(range: Rect) extends Command {
    override def write(index: Int, array: Array[Boolean]): Unit = {
      array(index) = !array(index)
    }
  }

  trait InstructionParser extends RegexParsers {
    override def skipWhitespace = false

    def num: Parser[Int] = """\d+""".r ^^ { case i => i.toInt }

    def pair: Parser[(Int, Int)] = num ~ "," ~ num ^^ { case x ~ _ ~ y => (x, y) }

    def range: Parser[Rect] = pair ~ " through " ~ pair ^^ { case tl ~ _ ~ br => Rect.mkRect(tl._1, tl._2, br._1, br._2) }

    def turnOn: Parser[TurnOn] = "turn on" ~ " " ~ range ^^ { case _ ~ _ ~ range => TurnOn(range) }

    def turnOff: Parser[TurnOff] = "turn off" ~ " " ~ range ^^ { case _ ~ _ ~ range => TurnOff(range) }

    def toggle: Parser[Toggle] = "toggle" ~ " " ~ range ^^ { case _ ~ _ ~ range => Toggle(range) }

    def command: Parser[Command] = turnOn | turnOff | toggle
  }
}