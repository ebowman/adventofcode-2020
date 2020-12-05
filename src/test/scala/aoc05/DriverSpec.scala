package aoc05

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class DriverSpec extends AnyFlatSpec with Matchers with Driver {
  "SeatFinder" should "solve known test cases" in {
    SeatFinder("FBFBBFFRLR").solve shouldBe (44, 5)
    SeatFinder("BFFFBBFRRR").solve shouldBe (70, 7)
    SeatFinder("FFFBBBFRRR").solve shouldBe (14, 7)
    SeatFinder("BBFFBBFRLL").solve shouldBe (102, 4)
    SeatFinder("FBFBBFFRLR").id shouldBe 357
    SeatFinder("BFFFBBFRRR").id shouldBe 567
    SeatFinder("FFFBBBFRRR").id shouldBe 119
    SeatFinder("BBFFBBFRLL").id shouldBe 820
  }

  it should "solve the first puzzle" in {
    Loader(this, "input.txt").map(id => SeatFinder(id)).maxBy(_.id).id shouldBe 919
  }
  it should "solve the second part" in {
    val tmp = Loader(this, "input.txt").map(id => SeatFinder(id)).map(sf => (sf.id, sf.code)).toMap
    val id = (for (i <- 80 to 919 if !tmp.contains(i)) yield i).head
    id shouldBe 642
  }

}
