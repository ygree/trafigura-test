package xxx.chesstask

import org.scalatest._
import impl.{CombinationFinderOnFutures, CombinationFinderSequential, FigurePlacementCounter}
import model._

class CombinationFinderSpec extends FlatSpec with Matchers {
  import Figure._
  import Position._

  def countAllAllowedDistinctCombinations(positions: List[Position], figures: List[Figure]): Long = {
    new CombinationFinderSequential().find(new FigurePlacementCounter)(positions, figures)
  }

  def countAllAllowedDistinctCombinationsPar(positions: List[Position], figures: List[Figure]): Long = {
    new CombinationFinderOnFutures().find(new FigurePlacementCounter)(positions, figures)
  }

  "CombinationFinderSequential" should "" in {
    val placementFinder = new FigurePlacementCounter
    val combinationFinder = new CombinationFinderSequential()
    val find2 = combinationFinder.find(placementFinder) _

    find2(allPositionsOrdered(3, 3), List(K, K, R)) should be (4)
    find2(allPositionsOrdered(4, 4), List(R, R, N, N, N, N)) should be (8)
  }

  "Test generating combinations" should "" in {


//    countAllAllowedDistinctCombinations(allPositionsOrdered(3, 3), List(K, K, R)) should be(4)
//    countAllAllowedDistinctCombinationsPar(allPositionsOrdered(4, 4), List(R, R, N, N)) should be(8)

//    val startAt = System.currentTimeMillis()
    //    countAllAllowedDistinctCombinationsPar(generateCoordinates(10, 8), List(K, K, R, R, R, K)) // should be (4)
//    countAllAllowedDistinctCombinations(allPositionsOrdered(6, 9), List(K, K, R, R, R, K)) // should be (4)
//    val endAt = System.currentTimeMillis()
//    println("total time (sec):" + (endAt - startAt) / 1000)
  }
}