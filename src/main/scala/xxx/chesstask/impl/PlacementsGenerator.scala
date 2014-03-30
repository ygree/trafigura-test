/*
 * (c) 2014, Gribkov Yury
 * http://linkedin.com/in/ygribkov/
 */

package xxx.chesstask.impl

import xxx.chesstask.model.Figure

object PlacementsGenerator {

  def apply(figures: Seq[Figure]): PlacementsGenerator = {
    val figureToNumber = figures.groupBy(identity) map {
      case (k, v) => (k, v.size)
    }
    new PlacementsGenerator(figureToNumber)
  }
}

class PlacementsGenerator(figuresPool: Map[Figure, Int]) {
  def uniquePlacements: Set[List[Figure]] =
    uniqueFigures match {
      case s if s.isEmpty => Set(Nil)
      case availableUniqueSymbols =>
        for {
          e <- availableUniqueSymbols
          tail <- excludeOne(e).uniquePlacements
        } yield e :: tail
    }

  def uniqueFigures: Set[Figure] = figuresPool.keySet

  def excludeOne(f: Figure): PlacementsGenerator = {
    val number = figuresPool(f)
    new PlacementsGenerator(
      if (number > 1) figuresPool.updated(f, number - 1)
      else figuresPool - f
    )
  }
}