package aoc03

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class DriverSpec extends AnyFlatSpec with Matchers {
  val testInput =
    """
      |..##.......
      |#...#...#..
      |.#....#..#.
      |..#.#...#.#
      |.#...##..#.
      |..#.##.....
      |.#.#.#....#
      |.#........#
      |#.##...#...
      |#...##....#
      |.#..#...#.#""".stripMargin.trim.split("\n").toIndexedSeq

  val liveInput = util.Loader(this, "input.txt").toIndexedSeq

  import Driver._
  val testBoards = Seq(Board(testInput), Board(testInput, 1, 1), Board(testInput, 5, 1), Board(testInput, 7, 1), Board(testInput, 1, 2))
  val liveBoards = Seq(Board(liveInput), Board(liveInput, 1, 1), Board(liveInput, 5, 1), Board(liveInput, 7, 1), Board(liveInput, 1, 2))

  "Driver" should "do pass the given test case" in {
    val board = Board(testInput)
    recurse(Cursor(board)) shouldBe 7
  }

  it should "solve the puzzle (part 1)" in {
    val board = Board(liveInput)
    recurse(Cursor(board)) shouldBe 176
  }

  it should "solve the second puzzle test case" in {
    testBoards.map { board => recurse(Cursor(board)) }.foldLeft(1) {
      case (a: Int, b: Int) => a * b
    } shouldBe 336
  }

  it should "solve the second puzzle live case" in {
    liveBoards.map { board => recurse(Cursor(board)) }.foldLeft(1L) {
      case (a: Long, b: Int) => a * b
    } shouldBe 5872458240L
  }
}
