package xxx.chesstask

import xxx.chesstask.Position._

trait FigurePlacementFinder[T] {

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
          newAvailablePosition = availableOrderedPositions dropWhile (_ < p)
        } yield find(newAvailablePosition, restFigures, newCombination)
        fold(result)
    }
}