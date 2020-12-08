package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day08Spec extends AnyFlatSpec with Matchers with Day08 {
  lazy val testInput =
    """
      |nop +0
      |acc +1
      |jmp +4
      |acc +3
      |jmp -3
      |acc -99
      |acc +1
      |jmp -4
      |acc +6""".stripMargin.trim.linesIterator.toIterable

  it should "pass the test case" in {
    Machine(new Compiler().compile(testInput)).run shouldBe(false, 5)
  }

  it should "solve the puzzle" in {
    new Machine(new Compiler().compile(Loader(this, "day08.txt"))).run shouldBe(false, 1262)
  }

  it should "solve the second test case" in {
    MetaMachine(new Compiler().compile(testInput)).run shouldBe 8

  }
  it should "solve the second puzzle" in {
    new MetaMachine(new Compiler().compile(Loader(this, "day08.txt"))).run shouldBe 1643
  }
}
