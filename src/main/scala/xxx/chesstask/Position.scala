package xxx.chesstask

object Position {

  type Position = (Int, Int)
  implicit def positionOrderingOps(v: Position) = implicitly[Ordering[Position]].mkOrderingOps(v)

  def allPositionsOrdered(m: Int, n: Int): List[Position] = for (i <- (0 until m).toList; j <- 0 until n) yield (i, j)
}