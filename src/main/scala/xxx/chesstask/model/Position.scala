/*
 * (c) 2014, Gribkov Yury
 * http://linkedin.com/in/ygribkov/
 */

package xxx.chesstask.model

object Position {

  type Position = (Int, Int)
  implicit def positionOrderingOps(v: Position) = implicitly[Ordering[Position]].mkOrderingOps(v)

  def allPositionsOrdered(m: Int, n: Int): List[Position] = for (i <- (0 until m).toList; j <- 0 until n) yield (i, j)
}