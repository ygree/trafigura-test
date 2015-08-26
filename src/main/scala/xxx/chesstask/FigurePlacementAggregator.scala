/*
 * (c) 2014, Gribkov Yury
 * http://linkedin.com/in/ygribkov/
 */

package xxx.chesstask

import model.{Figure, Position}
import Position._

trait FigurePlacementAggregator[T] {

  def emptyCombination: Combination

  def found(combination: Combination): T

  def fold(values: Seq[T]): T

  /**
   * finds all valid combinations for fixed sequence of figures
   */
  def find(availableOrderedPositions: List[Position], figures: List[Figure], combination: Combination = emptyCombination): T =
    figures match {
      case Nil => found(combination)
      case f :: restFigures =>
        val result = for {
          p <- availableOrderedPositions
          newCombination <- combination.place(p, f)
          newAvailablePosition = availableOrderedPositions dropWhile (_ <= p)
        } yield find(newAvailablePosition, restFigures, newCombination)
        fold(result)
    }
}