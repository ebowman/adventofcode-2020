package aoc01

import scala.io.Source

object Sum2020 extends App {
  val inputs = Source.fromInputStream(getClass.getResourceAsStream("input.txt")).getLines().map(_.toInt).toSet

  for (a <- inputs) {
    val b = 2020 - a
    if (inputs.contains(b)) println(a * b)
  }
}
