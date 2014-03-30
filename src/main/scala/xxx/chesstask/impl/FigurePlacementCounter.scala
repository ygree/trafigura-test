/*
 * (c) 2014, Gribkov Yury
 * http://linkedin.com/in/ygribkov/
 */

package xxx.chesstask.impl

import xxx.chesstask.{Combination, FigurePlacementAggregator}

class FigurePlacementCounter extends FigurePlacementAggregator[Long] {

  def emptyCombination: Combination = new CombinationByFun()

  def fold(values: Seq[Long]): Long = values.sum

  def found(combination: Combination): Long = 1
}