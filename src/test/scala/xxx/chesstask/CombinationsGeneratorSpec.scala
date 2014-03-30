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

  def countAllAllowedDistinctCombinations(positions: List[Position], figures: List[Figure]): Long = {
    new CombinatorFinderSequential().find(positions, figures)(new FigurePlacementCounter)
  }

  def countAllAllowedDistinctCombinationsPar(positions: List[Position], figures: List[Figure]): Long = {
    new CombinatorFinderOnFutures().find(positions, figures)(new FigurePlacementCounter)
  }

  "Test generating combinations" should "" in {


    countAllAllowedDistinctCombinations(allPositionsOrdered(3, 3), List(K, K, R)) should be(4)

    val startAt = System.currentTimeMillis()
    //    countAllAllowedDistinctCombinationsPar(generateCoordinates(10, 8), List(K, K, R, R, R, K)) // should be (4)
    countAllAllowedDistinctCombinations(allPositionsOrdered(6, 9), List(K, K, R, R, R, K)) // should be (4)
    val endAt = System.currentTimeMillis()
    println("total time (sec):" + (endAt - startAt) / 1000)
  }
}