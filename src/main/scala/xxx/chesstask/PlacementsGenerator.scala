package xxx.chesstask

object PlacementsGenerator {

  def apply(figures: Seq[Figure]): PlacementsGenerator = {
    val figureToNumber = figures.groupBy(identity) map {
      case (k, v) => (k, v.size)
    }
    new PlacementsGenerator(figureToNumber)
  }
}

class PlacementsGenerator(figureToNumber: Map[Figure, Int]) {
  def generateUniquePlacements: Set[List[Figure]] =
    uniqueSymbols match {
      case s if s.isEmpty => Set(Nil)
      case availableUniqueSymbols =>
        for {
          e <- availableUniqueSymbols
          tail <- excludeOne(e).generateUniquePlacements
        } yield e :: tail
    }

  def uniqueSymbols: Set[Figure] = figureToNumber.keySet

  def excludeOne(f: Figure): PlacementsGenerator = {
    val number = figureToNumber(f)
    new PlacementsGenerator(
      if (number > 1) figureToNumber.updated(f, number - 1)
      else figureToNumber - f
    )
  }
}
