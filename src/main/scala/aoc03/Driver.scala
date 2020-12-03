package aoc03

object Driver {

  val defaultSlope: (Int, Int) = (3, 1)

  case class Board(rows: IndexedSeq[String], slope: (Int, Int) = defaultSlope) {
    private val width: Int = rows(0).length

    def solve: Int = Cursor().solve()

    private case class Cursor(x: Int = 0, y: Int = 0) {
      def next: Option[Cursor] = {
        if (y + slope._2 >= rows.length) None
        else Some(Cursor((x + slope._1) % width, y + slope._2))
      }

      def tree: Int = if (rows(y)(x) == '#') 1 else 0

      @scala.annotation.tailrec
      final def solve(trees: Int = 0): Int = {
        val t = trees + tree
        val n = next
        if (n.nonEmpty) n.get.solve(t)
        else t
      }
    }

  }

}
