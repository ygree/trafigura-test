package xxx.chesstask.impl

import xxx.chesstask.{CombinationFinder, FigurePlacementAggregator}
import xxx.chesstask.model.Figure
import scala.concurrent.{Await, Future, ExecutionContext}

class CombinationFinderSequential extends CombinationFinder {
  def find[T](finder: FigurePlacementAggregator[T])(positions: List[(Int, Int)], figures: List[Figure]): T = {
    val result = PlacementsGenerator(figures).uniquePlacements.toSeq map { uniqueFigureCombination =>
      finder.find(positions, uniqueFigureCombination)
    }
    finder.fold(result)
  }
}

class CombinationFinderOnFutures extends CombinationFinder {
  def find[T](finder: FigurePlacementAggregator[T])(positions: List[(Int, Int)], figures: List[Figure]): T = {
    import scala.concurrent.duration._
    import ExecutionContext.Implicits.global
    val result = PlacementsGenerator(figures).uniquePlacements.toSeq map { uniqueFigureCombination =>
      Future(finder.find(positions, uniqueFigureCombination))
    }
    val future = Future.sequence(result).map(finder.fold)
    Await.result(future, Duration.Inf)
  }
}