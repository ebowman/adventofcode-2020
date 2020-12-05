package aoc05

import scala.annotation.tailrec

trait Driver {

  case class SeatFinder(code: String, rowMin: Int = 0, rowMax: Int = 127, colMin: Int = 0, colMax: Int = 7) {
    def next: Option[SeatFinder] = {
      if (code.isEmpty) None
      else {
        code.head match {
          case 'F' => Some(copy(code = code.tail, rowMax = rowMax - (rowMax - rowMin ) / 2 - 1))
          case 'B' => Some(copy(code = code.tail, rowMin = rowMin + (rowMax - rowMin) / 2 + 1))
          case 'L' => Some(copy(code = code.tail, colMax = colMax - (colMax - colMin) / 2 - 1))
          case 'R' => Some(copy(code = code.tail, colMin = colMin + (colMax - colMin) / 2 + 1))
        }
      }
    }

    def id = {
      val (row, col) = solve
      row * 8 + col
    }

    def solve: (Int, Int) = {
      @tailrec
      def recurse(f: SeatFinder): SeatFinder = {
        f.next match {
          case Some(n) => recurse(n)
          case None => f
        }
      }

      val done = recurse(this)
      assert(done.rowMin == done.rowMin, s"done = $done")
      assert(done.colMin == done.colMax, s"done = $done")
      (done.rowMin, done.colMin)
    }
  }

}
