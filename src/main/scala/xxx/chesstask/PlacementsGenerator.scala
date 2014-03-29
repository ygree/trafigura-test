package xxx.chesstask

object PlacementsGenerator {
  
  def generateUniquePlacements(ss: List[Figure]): Set[List[Figure]] =
    Symbols.generateUniquePlacements(Symbols(ss))
  
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
  
  private object Symbols{
    def apply(figures: Seq[Figure]): Symbols = {
      val figureToNumber = figures.groupBy(identity) map { 
        case (k, v) => (k, v.size)
      }
      new Symbols(figureToNumber)
    }
    def generateUniquePlacements(ss: Symbols): Set[List[Figure]] =
      ss.uniqueSymbols match {
        case s if s.isEmpty => Set(Nil)
        case availableUniqueSymbols  =>
	      for {
	        e <- availableUniqueSymbols
	        tail <- generateUniquePlacements(ss excludeOne e)
	      } yield e :: tail
      }
  }
}