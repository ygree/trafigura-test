package xxx.chesstask

import org.scalatest._

class CombinationsGeneratorSpec extends FlatSpec with Matchers {

  import PlacementsGenerator._
  import Figure._  
  
  def generateCoordinates(m: Int, n: Int): List[Coords] =
    for (i <- (0 until m).toList; j <- 0 until n) yield Coords(i, j)
  
  "Test generating coordinates" should "" in {
    val result = generateCoordinates(6, 9)
    result.size should be (6 * 9)
    result.size should be (result.toSet.size)
  }

  
  type CheckPosition = PartialFunction[Coords, Boolean]
  
  def kingThretens(fpos: Coords) = { p: Coords => 
    (Math.abs(fpos.m - p.m) <= 1) && (Math.abs(fpos.n - p.n) <= 1)
  }
  
  def checkIfPossitionNotUnderThreten(f: Figure, fpos: Coords): CheckPosition = {
    val isThretensFun = f match {
      case K => kingThretens(fpos) 
    }
    {
      case p if isThretensFun(p) => false
    }
  }
  
  class Combination private (checkIfAllowedPosition: CheckPosition, pos: Set[Coords] = Set()) {
    def place(c: Coords, f: Figure): Option[Combination] =
      if (!checkIfAllowedPosition(c)) {
        println("when " + pos + " => rejected: " + c)
        None 
      }
      else {
//        val positionTakenFun: CheckPosition = { case `c` => false }
//        		val newFun = checkIfPossitionUnderThreten(f, c) orElse positionTakenFun orElse checkIfAllowedPosition
        val newFun = checkIfPossitionNotUnderThreten(f, c) orElse checkIfAllowedPosition
        Some(new Combination(newFun, pos + c))
      }
    
    def print() = println(pos)
  }
  
  object Combination {
    def apply(): Combination = new Combination({ case _ => true })
  }
  
  def countAllowedCombinations(availablePositions: Seq[Coords], figures: List[Figure], combination: Combination = Combination()): Int = {
    figures match {
      case Nil =>
        combination.print()
        1
    		  
      case f :: restFigures =>
	    val a = for {
	      p <- availablePositions //.toSeq.sortWith(_ < _)
	      newCombination <- combination.place(p, f)
	      newAvailablePosition = availablePositions filter (p < _)
//	      _ = println("p>" + p)
//	      _ = println("a>" + newAvailablePosition.toSeq.sortWith(_ < _))
	    } yield countAllowedCombinations(newAvailablePosition, restFigures, newCombination)
	    a.sum
    }
  }

  "Test generating combinations" should "" in {
    
    (Coords(0, 0) < Coords(1, 0)) should be (true)
    (Coords(0, 0) < Coords(0, 1)) should be (true)
    (Coords(1, 0) < Coords(0, 1)) should be (false)
    (Coords(1, 1) < Coords(1, 0)) should be (false)
    
    kingThretens(Coords(0,0))(Coords(1,2)) should be (false)

    kingThretens(Coords(1,1))(Coords(0,0)) should be (true)
    kingThretens(Coords(1,1))(Coords(0,1)) should be (true)
    kingThretens(Coords(1,1))(Coords(0,2)) should be (true)
    
    kingThretens(Coords(1,1))(Coords(1,0)) should be (true)
//    kingThretens(Coords(1,1))(Coords(1,1)) should be (false)
    kingThretens(Coords(1,1))(Coords(1,2)) should be (true)
    
    kingThretens(Coords(1,1))(Coords(2,0)) should be (true)
    kingThretens(Coords(1,1))(Coords(2,1)) should be (true)
    kingThretens(Coords(1,1))(Coords(2,2)) should be (true)
    
    kingThretens(Coords(1,1))(Coords(1,3)) should be (false)
    
//    countAllowedCombinations(generateCoordinates(1, 1).toSet, Nil) should be (1)
//    countAllowedCombinations(generateCoordinates(1, 1).toSet, List(K)) should be (1)
    
//    countAllowedCombinations(generateCoordinates(1, 2).toSet, List(K)) should be (2)
//    countAllowedCombinations(generateCoordinates(2, 2).toSet, List(K, K)) should be (0)
//    generateCoordinates(2, 3).toSet.filter(Coords(0,2) < _) should be (Set())
    
    countAllowedCombinations(generateCoordinates(2, 3), List(K, K)) should be (4)
//    countAllowedCombinations(generateCoordinates(3, 3).toSet, List(K)) should be (9)
//    countAllowedCombinations(generateCoordinates(3, 3).toSet, List(K, K)) should be (15)
    
//    generateCombinations(coords, 1, _ => true).size should be (54)
    
  }
}