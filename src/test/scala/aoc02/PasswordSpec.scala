package aoc02

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class ParserSpec extends AnyFlatSpec with Matchers with aoc02.Common {

  case class AllCapsChecker(c: String, a: Int, b: Int) extends Rule {
    def check(password: String): Boolean = password.capitalize == password
  }

  override val parser = new InputParser(AllCapsChecker.apply)

  "Parsing" should "pass basic tests" in {
    parser.parseAll(parser.num, "2").get shouldBe 2
    parser.parseAll(parser.rule, "1-3 a").get.check("HELLO") shouldBe true
  }
}

trait InputSource {
  val input: Seq[String] =
    """
      |1-3 a: abcde
      |1-3 b: cdefg
      |2-9 c: ccccccccc
      |""".stripMargin.split("\n").filter(_.nonEmpty).toSeq
}

class PasswordSpec extends AnyFlatSpec with Matchers with aoc02.Common with InputSource {

  val parser = Passwords.parser.asInstanceOf[InputParser]

  "Checker" should "pass basic tests" in {
    count(input) shouldBe 2
  }

  it should "print out the result we hope to get" in {
    println(Passwords.result)
  }

  it should "solve the final puzzle" in {
    Passwords.result shouldBe 591
  }
}

class Password2Spec extends AnyFlatSpec with Matchers with aoc02.Common with InputSource {

  val parser = Passwords2.parser.asInstanceOf[InputParser]

  "Checker2" should "pass the examples" in {
    count(input) shouldBe 1
  }

  it should "print out the result we hope to get" in {
    println(Passwords2.result)
  }

  it should "solve the final puzzle" in {
    Passwords2.result shouldBe 335
  }
}
