
/*
--- Day 1: Report Repair ---

After saving Christmas five years in a row, you've decided to take a vacation at a nice resort on a tropical island.
Surely, Christmas will go on without you.

The tropical island has its own currency and is entirely cash-only. The gold coins used there have a little picture
of a starfish; the locals just call them stars. None of the currency exchanges seem to have heard of them, but somehow,
you'll need to find fifty of these coins by the time you arrive so you can pay the deposit on your room.

To save your vacation, you need to get all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar;
the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!

Before you leave, the Elves in accounting just need you to fix your expense report (your puzzle input);
apparently, something isn't quite adding up.

Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.

For example, suppose your expense report contained the following:

1721
979
366
299
675
1456

In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579,
so the correct answer is 514579.

Of course, your expense report is much larger. Find the two entries that sum to 2020;
what do you get if you multiply them together?

Your puzzle answer was 987339.

--- Part Two ---

The Elves in accounting are thankful for your help; one of them even offers you a starfish coin they had left over
from a past vacation. They offer you a second one if you can find three numbers in your expense report that meet
the same criteria.

Using the above example again, the three entries that sum to 2020 are 979, 366, and 675. Multiplying them together
produces the answer, 241861950.

In your expense report, what is the product of the three entries that sum to 2020?

Your puzzle answer was 259521570.

Both parts of this puzzle are complete! They provide two gold stars: **

At this point, you should return to your Advent calendar and try another puzzle.

If you still want to see it, you can get your puzzle input.
 */
package aoc01

object Sum2020 {

  /** Load the custom inputs (from src/main/resources/aoc02/input.txt) into a Set[Int] */
  lazy val inputs = {
    import scala.io.Source
    Source.fromInputStream(getClass.getResourceAsStream("input.txt")).getLines().map(_.toInt).toSet
  }

  /**
   * Given a Set[Int], assumes there are two values within it that sum to 2020. Finds them and returns their product.
   * If the set does not include two values that add up to 2020, will blow come kind of runtime exception as it tries
   * to pull the head off an empty collection.
   */
  def s2(inputs: Set[Int]): Int = {
    inputs.flatMap { a =>
      inputs.find(_ == 2020 - a).map(_ * a)
    }.head
  }

  lazy val sum2 = s2(inputs)

  /** Similar to s2, but this assumes there are 3 values which together add up to 2020, and returns their product. */
  def s3(inputs: Set[Int]): Int = {
    inputs.flatMap { a =>
      inputs.withFilter(_ != a).flatMap { b =>
        inputs.find(_ == 2020 - a - b).map(_ * b * a)
      }
    }.head
  }

  lazy val sum3 = s3(inputs)
}
