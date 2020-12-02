package aoc01

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Sum2020Spec extends AnyFlatSpec with Matchers {

  "Sum2020" should "confirm the example in the docs" in {
    val inputs = Set(1721, 979, 366, 299, 675, 1456)
    Sum2020.s2(inputs) shouldBe 514579
    Sum2020.s3(inputs) shouldBe 241861950
  }

  it should "print out the result(s) we hope to get" in {
    println(Sum2020.sum2)
    println(Sum2020.sum3)
  }

  it should "get the right answer for my personal inputs" in {
    Sum2020.sum2 shouldBe 987339
    Sum2020.sum3 shouldBe 259521570
  }
}
