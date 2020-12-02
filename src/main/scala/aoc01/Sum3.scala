package aoc01

import scala.io.Source

/** For day 2, we are looking for 3 numbers that sum to 2020 */
object Sum3 extends App {
  val inputs = Source.fromInputStream(getClass.getResourceAsStream("input.txt")).getLines().map(_.toInt).toSet

  for (a <- inputs) {
    for (b <- inputs if a != b) {
      val c = 2020 - a - b
      if (inputs.contains(c)) println(a * b * c)
    }
  }

}
