package aoc03

object Driver {
  lazy val input: Seq[String] = util.Loader(this, "input.txt").toIndexedSeq

  case class Board(rows: IndexedSeq[String], ruleX: Int = 3, ruleY: Int = 1) {
    val width: Int = rows(0).length
  }

  case class Cursor(board: Board, x: Int = 0, y: Int = 0) {
    def next: Option[Cursor] = {
      if (y + board.ruleY >= board.rows.length) None
      else Some(Cursor(board, (x + board.ruleX) % board.width, y + board.ruleY))
    }
    def tree: Int = if (board.rows(y)(x) == '#') 1 else 0
  }

  @scala.annotation.tailrec
  def recurse(cursor: Cursor, trees: Int = 0): Int = {
    val t = trees + cursor.tree
    val n = cursor.next
    if (n.nonEmpty) recurse(n.get, t)
    else t
  }

}
