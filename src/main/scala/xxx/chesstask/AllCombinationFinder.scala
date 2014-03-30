package xxx.chesstask

import xxx.chesstask.Position._
import scala.concurrent.{Await, Future, ExecutionContext}

trait AllCombinationFinder {

  //TODO consider using SortedSet for positions here
  def count[T](orderedPositions: List[Position], figureSet: List[Figure])(finder: FigurePlacementFinder[T]): T
}

class CombinatorFinderSequentialAll extends AllCombinationFinder {
  def count[T](positions: List[(Int, Int)], figures: List[Figure])(finder: FigurePlacementFinder[T]): T = {
    val result = PlacementsGenerator(figures).uniquePlacements.toSeq map {
      uniqueFigureCombination =>
        finder.find(positions, uniqueFigureCombination)
    }
    finder.fold(result)
  }
}

class CombinatorFinderConcurrentAll extends AllCombinationFinder {
  def count[T](positions: List[(Int, Int)], figures: List[Figure])(finder: FigurePlacementFinder[T]): T = {
    import scala.concurrent.duration._
    import ExecutionContext.Implicits.global
    val result = PlacementsGenerator(figures).uniquePlacements.toSeq map {
      uniqueFigureCombination =>
        Future(finder.find(positions, uniqueFigureCombination))
    }
    val future = Future.sequence(result).map(finder.fold)
    Await.result(future, Duration.Inf)
  }
}