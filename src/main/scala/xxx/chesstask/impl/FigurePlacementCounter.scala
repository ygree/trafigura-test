package xxx.chesstask.impl

import xxx.chesstask.{Combination, FigurePlacementFinder}

class FigurePlacementCounter extends FigurePlacementFinder[Long] {

  def emptyCombination: Combination = new CombinationByFunCheckOnly()

  def fold(values: Seq[Long]): Long = values.sum

  def found(combination: Combination): Long = 1
}