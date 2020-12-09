package y2020

trait Day09 {

  def part1(preamble: IndexedSeq[Long], count: Int, start: IndexedSeq[Long]): Long = {
    val combs = preamble.take(count).combinations(2)
    combs.map(ab => ab(0) + ab(1)).find(_ == start(0)) match {
      case Some(_) => part1(preamble.tail, count, start.tail)
      case None => start.head
    }
  }

  def part2(data: IndexedSeq[Long], goal: Long): Long = {
    (for {
      a <- data.indices
      b <- 2 to data.length if data.slice(a, a + b).sum == goal
    } yield {
      val slice = data.slice(a, a+b)
      slice.min + slice.max
    }).head
  }
}
