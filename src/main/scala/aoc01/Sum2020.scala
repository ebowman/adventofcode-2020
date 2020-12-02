package aoc01

import scala.io.Source

object Sum2020 extends App {

  lazy val inputs = Source.fromInputStream(getClass.getResourceAsStream("input.txt")).getLines().map(_.toInt).toSet

  lazy val sum2 = {
    inputs.flatMap { a =>
      val b = 2020 - a
      if (inputs.contains(b)) Some(a * b) else None
    }.head
  }

  println(sum2)

  lazy val sum3 = {
    inputs.flatMap { a =>
      inputs.withFilter(b => a != b).flatMap { b =>
        val c = 2020 - a - b
        if (inputs.contains(c)) Some(a * b * c) else None
      }
    }.head
  }

  println(sum3)
}
