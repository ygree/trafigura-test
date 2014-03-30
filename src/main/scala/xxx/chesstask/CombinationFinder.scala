package xxx.chesstask

import model.{Figure, Position}
import Position._

trait CombinationFinder {

  //TODO consider using SortedSet for positions here
  def find[T](orderedPositions: List[Position], figureSet: List[Figure])(finder: FigurePlacementFinder[T]): T
}
