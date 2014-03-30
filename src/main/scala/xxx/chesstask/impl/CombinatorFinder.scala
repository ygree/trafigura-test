package xxx.chesstask.impl

import xxx.chesstask.{CombinationFinder, FigurePlacementFinder}
import xxx.chesstask.model.Figure
import scala.concurrent.{Await, Future, ExecutionContext}

class CombinatorFinderSequential extends CombinationFinder {
  def find[T](positions: List[(Int, Int)], figures: List[Figure])(finder: FigurePlacementFinder[T]): T = {
    val result = PlacementsGenerator(figures).uniquePlacements.toSeq map { uniqueFigureCombination =>
      finder.find(positions, uniqueFigureCombination)
    }
    finder.fold(result)
  }
}

class CombinatorFinderOnFutures extends CombinationFinder {
  def find[T](positions: List[(Int, Int)], figures: List[Figure])(finder: FigurePlacementFinder[T]): T = {
    import scala.concurrent.duration._
    import ExecutionContext.Implicits.global
    val result = PlacementsGenerator(figures).uniquePlacements.toSeq map { uniqueFigureCombination =>
      Future(finder.find(positions, uniqueFigureCombination))
    }
    val future = Future.sequence(result).map(finder.fold)
    Await.result(future, Duration.Inf)
  }
}