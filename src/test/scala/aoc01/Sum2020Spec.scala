package aoc01
import org.scalatest._
import matchers.should._
import org.scalatest.flatspec.AnyFlatSpec

class Sum2020Spec extends AnyFlatSpec with Matchers {

  "Sum2020" should "get the right answer" in {
    Sum2020.sum2 shouldBe 987339
    Sum2020.sum3 shouldBe 259521570
  }
}
