/*
 * (c) 2014, Gribkov Yury
 * http://linkedin.com/in/ygribkov/
 */

package xxx.chesstask

import org.scalatest._
import impl.{CombinationFinderOnFutures, CombinationFinderSequential, FigurePlacementCounter}
import model._

class CombinationFinderSpec extends FlatSpec with Matchers {
  import Figure._
  import Position._

  "CombinationFinderSequential" should "solve task examples" in {
    checkTaskExamples(new CombinationFinderSequential())
  }

  "CombinationFinderOnFutures" should "solve task examples" in {
    checkTaskExamples(new CombinationFinderOnFutures())
  }

  def checkTaskExamples(combinationFinder: CombinationFinder): Unit = {
    val placementFinder = new FigurePlacementCounter
    val find = combinationFinder.find(placementFinder) _

    find(allPositionsOrdered(3, 3), List(K, K, R)) should be (4)
    find(allPositionsOrdered(4, 4), List(R, R, N, N, N, N)) should be (8)
  }
}