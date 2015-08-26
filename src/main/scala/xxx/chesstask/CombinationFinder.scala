/*
 * (c) 2014, Gribkov Yury
 * http://linkedin.com/in/ygribkov/
 */

package xxx.chesstask

import model.{Figure, Position}
import Position._

trait CombinationFinder {

  //TODO consider using SortedSet for positions here
  def find[T](finder: FigurePlacementAggregator[T])(orderedPositions: List[Position], figureSet: List[Figure]): T
}