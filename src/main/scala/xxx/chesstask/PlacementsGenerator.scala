package xxx.chesstask

object PlacementsGenerator {

  def excludeFollowingDups(ss: List[Figure], used: Set[Figure] = Set()): List[Figure] =
    ss match {
      case Nil                          => Nil
      case h :: tail if used contains h => excludeFollowingDups(tail, used + h)
      case h :: tail                    => h :: excludeFollowingDups(tail, used + h)
    }

//  def generateUniquePlacements(ss: List[Figure]): List[List[Figure]] = ???

  def generateUniquePlacements(ss: Array[Figure], excludeIndexes: Set[Int] = Set()): Seq[List[Figure]] =
    for {
      i <- 0 until ss.size if !(excludeIndexes contains i)
      tail <- generateUniquePlacements(ss, excludeIndexes + i)
    } yield ss(i) :: tail


}
