package xxx.chesstask

import org.scalatest._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.Await
import xxx.chesstask.impl.{CombinatorFinderOnFutures, CombinatorFinderSequential, FigurePlacementCounter}
import xxx.chesstask.model.{Position, Figure}

class CombinationsGeneratorSpec extends FlatSpec with Matchers {
  import Figure._
  import Position._

  def countAllowedCombinations(availablePositions: List[Position], figures: List[Figure]): Long = {
    new FigurePlacementCounter().find(availablePositions, figures)
  }


  def countAllAllowedDistinctCombinations(positions: List[Position], figures: List[Figure]): Long = {
    new CombinatorFinderSequential().find(positions, figures)(new FigurePlacementCounter)
  }

  def countAllAllowedDistinctCombinationsPar(positions: List[Position], figures: List[Figure]): Long = {
    new CombinatorFinderOnFutures().find(positions, figures)(new FigurePlacementCounter)
  }

  "Test generating combinations" should "" in {

    //    countAllowedCombinations(generateCoordinates(1, 1).toSet, Nil) should be (1)
    //    countAllowedCombinations(generateCoordinates(1, 1).toSet, List(K)) should be (1)

    //    countAllowedCombinations(generateCoordinates(1, 2).toSet, List(K)) should be (2)
    //    countAllowedCombinations(generateCoordinates(2, 2).toSet, List(K, K)) should be (0)
    //
    countAllowedCombinations(allPositionsOrdered(2, 3), List(K, K)) should be(4)
    //    countAllowedCombinations(generateCoordinates(3, 3).toSet, List(K)) should be (9)

    countAllowedCombinations(allPositionsOrdered(3, 3), List(K, K)) should be(16)
    //
    countAllowedCombinations(allPositionsOrdered(2, 2), List(R, R)) should be(2)
    countAllowedCombinations(allPositionsOrdered(2, 3), List(R, R)) should be(6)

    countAllAllowedDistinctCombinations(allPositionsOrdered(3, 3), List(K, K, R)) should be(4)

    val startAt = System.currentTimeMillis()
    //    countAllAllowedDistinctCombinationsPar(generateCoordinates(10, 8), List(K, K, R, R, R, K)) // should be (4)
    countAllAllowedDistinctCombinations(allPositionsOrdered(6, 9), List(K, K, R, R, R, K)) // should be (4)
    val endAt = System.currentTimeMillis()
    println("total time (sec):" + (endAt - startAt) / 1000)
  }
}