package xxx.chesstask

import model.{Figure, Position}
import Position._

trait CombinationFinder {

  //TODO consider using SortedSet for positions here
  def find[T](finder: FigurePlacementFinder[T])(orderedPositions: List[Position], figureSet: List[Figure]): T
}