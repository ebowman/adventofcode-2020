package aoc02

import org.scalatest._
import matchers.should._
import org.scalatest.flatspec.AnyFlatSpec


class PasswordSpec extends AnyFlatSpec with Matchers {
  "Checker" should "pass basic tests" in {
    import Passwords.Checker
    Checker("a", 0, 2).check("b") shouldBe true
    Checker("a", 0, 2).check("ba") shouldBe true
    Checker("a", 0, 2).check("aba") shouldBe true
    Checker("a", 0, 2).check("abaca") shouldBe false

    Checker.makeChecker("1-3 z").check("zzz") shouldBe true
    Checker.makeChecker("1-3 z").check("zzzz") shouldBe false
  }

  it should "solve the final puzzle" in {
    Passwords.count(getClass.getResourceAsStream("input.txt")) shouldBe 591
  }

  "Checker2" should "pass the examples" in {
    import Passwords2.Checker

    // 1-3 a: abcde is valid: position 1 contains a and position 3 does not.
    // 1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
    // 2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.
    Checker.makeChecker("1-3 a").check("abcde") shouldBe true
    Checker.makeChecker("1-3 b").check("cdefg") shouldBe false
    Checker.makeChecker("2-9 c").check("ccccccccc") shouldBe false
  }

  it should "solve the final puzzle" in {
    Passwords2.count(getClass.getResourceAsStream("input.txt")) shouldNot equal(157)
  }


}
