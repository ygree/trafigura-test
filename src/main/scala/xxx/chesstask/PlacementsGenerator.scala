package xxx.chesstask

object PlacementsGenerator {

  def generateUniquePlacements(figures: List[Figure]): Set[List[Figure]] =
    Symbols.generateUniquePlacements(Symbols(figures))

  private object Symbols {
    def generateUniquePlacements(symbols: Symbols): Set[List[Figure]] =
      symbols.uniqueSymbols match {
        case s if s.isEmpty => Set(Nil)
        case availableUniqueSymbols =>
          for {
            e <- availableUniqueSymbols
            tail <- generateUniquePlacements(symbols excludeOne e)
          } yield e :: tail
      }

    def apply(figures: Seq[Figure]): Symbols = {
      val figureToNumber = figures.groupBy(identity) map {
        case (k, v) => (k, v.size)
      }
      new Symbols(figureToNumber)
    }
  }

  private class Symbols(figureToNumber: Map[Figure, Int]) {
    def excludeOne(f: Figure): Symbols = {
      val number = figureToNumber(f)
      new Symbols(
        if (number > 1) figureToNumber.updated(f, number - 1)
        else figureToNumber - f
      )
    }

    def uniqueSymbols: Set[Figure] = figureToNumber.keySet
  }
}