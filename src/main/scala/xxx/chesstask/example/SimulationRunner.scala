/*
 * (c) 2014, Gribkov Yury
 * http://linkedin.com/in/ygribkov/
 */

package xxx.chesstask.example

import xxx.chesstask.impl.FigurePlacementCounter
import xxx.chesstask.CombinationFinder
import xxx.chesstask.model._
import Position._
import Figure._

class SimulationRunner(combinationFinder: CombinationFinder)(
  positions: List[Position] = allPositionsOrdered(6, 9),
  figures: List[Figure] = List(K, K, Q, B, R, N)
) {
  def placementFinder = new FigurePlacementCounter

  def run(): Unit = {
    val startAt = System.currentTimeMillis()
    println("Running...")
    val found = combinationFinder.find(placementFinder)(positions, figures)
    println(s"Found solutions: $found")
    val endAt = System.currentTimeMillis()
    println("Total time (sec):" + (endAt - startAt) / 1000)
  }
}