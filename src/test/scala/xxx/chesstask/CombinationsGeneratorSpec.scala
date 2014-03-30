package xxx.chesstask

import org.scalatest._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.Await
import java.util.Date

class CombinationsGeneratorSpec extends FlatSpec with Matchers {

  import PlacementsGenerator._
  import Figure._

  def generateCoordinates(m: Int, n: Int): List[Position] =
    for (i <- (0 until m).toList; j <- 0 until n) yield Position(i, j)

  "Test generating coordinates" should "" in {
    val result = generateCoordinates(6, 9)
    result.size should be(6 * 9)
    result.size should be(result.toSet.size)
  }


  type CheckPosition = PartialFunction[Position, Boolean]

  def kingThretens(fpos: Position) = {
    p: Position =>
      (Math.abs(fpos.m - p.m) < 2) && (Math.abs(fpos.n - p.n) < 2)
  }

  def rookThretens(fpos: Position) = {
    p: Position =>
      fpos.m == p.m || fpos.n == p.n
  }

  def isThretensFun(f: Figure, fpos: Position) = f match {
    case K => kingThretens(fpos)
    case R => rookThretens(fpos)
  }

  //  def isPositionSafety(f: Figure, fpos: Coords): Boolean = isThretensFun(f, fpos)

  def isSafetyPositionFun(f: Figure, fpos: Position): CheckPosition = {
    case p if isThretensFun(f, fpos)(p) => false
  }

  class Combination private(checkIfAllowedPosition: CheckPosition, pos: List[Position] = Nil, figures: List[Figure] = Nil) {
    def place(c: Position, f: Figure): Option[Combination] =
      if (!checkIfAllowedPosition(c)) {
        //} || !pos.exists( checkIfPossitionNotUnderThreten(f, c) orElse { case _ => true })) {
        //        println("when " + pos + " => rejected: " + c)
        None
      }
      //      else if (pos.exists(rookThretens(c))) None
      else if (pos.exists(p => !(isSafetyPositionFun(f, c) orElse Combination.alwaysTrue)(p))) None
      //      else if (pos.nonEmpty && pos.forall(p => (isSafetyPositionFun(f, c) orElse Combination.alwaysTrue)(p))) None
      //      else if (pos.forall(isSafetyPositionFun(f, c) orElse { case _ => true })) None
      else {
        //        val positionTakenFun: CheckPosition = { case `c` => false }
        //        		val newFun = checkIfPossitionNotUnderThreten(f, c) orElse positionTakenFun orElse checkIfAllowedPosition
        //checkIfPossitionNotUnderThreten
        val newFun = isSafetyPositionFun(f, c) orElse checkIfAllowedPosition
        Some(new Combination(newFun, c :: pos, f :: figures))
      }

    def print() = println("found combination " + pos.reverse + " for " + figures.reverse)
  }

  object Combination {
    val alwaysTrue: CheckPosition = {
      case _ => true
    }

    def apply(): Combination = new Combination(alwaysTrue)
  }

  def countAllowedCombinations(availablePositions: List[Position], figures: List[Figure], combination: Combination = Combination()): Long = {
    figures match {
      case Nil =>
        //        combination.print()
        1

      case f :: restFigures =>
        val result = for {
          p <- availablePositions //.toSeq.sortWith(_ < _)
          newCombination <- combination.place(p, f)
          newAvailablePosition = availablePositions dropWhile (_ < p)
        //	      newAvailablePosition = availablePositions filter (p < _)
        //	      _ = println("p>" + p)
        //	      _ = println("a>" + newAvailablePosition.toSeq.sortWith(_ < _))
        } yield countAllowedCombinations(newAvailablePosition, restFigures, newCombination)
        result.sum
    }
  }

  def countAllAllowedDistinctCombinations(positions: List[Position], figures: List[Figure]): Long = {
    val result = PlacementsGenerator(figures).generateUniquePlacements.toSeq map {
      uniqueFigureCombination =>
        countAllowedCombinations(positions, uniqueFigureCombination)
    }
    result.sum
  }

  def countAllAllowedDistinctCombinationsPar(positions: List[Position], figures: List[Figure]): Long = {
    import scala.concurrent.duration._
    import ExecutionContext.Implicits.global
    val result = PlacementsGenerator(figures).generateUniquePlacements.toSeq map {
      uniqueFigureCombination =>
        Future(countAllowedCombinations(positions, uniqueFigureCombination))
    }
    val future = Future.sequence(result).map(_.sum)
    Await.result(future, Duration.Inf)
  }

  "Test generating combinations" should "" in {

    rookThretens(Position(0, 2))(Position(0, 0)) should be(true)

    Combination().place(Position(0, 0), K).get.place(Position(0, 2), R) should be(None)


    (Position(0, 0) < Position(1, 0)) should be(true)
    (Position(0, 0) < Position(0, 1)) should be(true)
    (Position(1, 0) < Position(0, 1)) should be(false)
    (Position(1, 1) < Position(1, 0)) should be(false)

    kingThretens(Position(0, 0))(Position(1, 2)) should be(false)

    kingThretens(Position(1, 1))(Position(0, 0)) should be(true)
    kingThretens(Position(1, 1))(Position(0, 1)) should be(true)
    kingThretens(Position(1, 1))(Position(0, 2)) should be(true)

    kingThretens(Position(1, 1))(Position(1, 0)) should be(true)
    //    kingThretens(Coords(1,1))(Coords(1,1)) should be (false)
    kingThretens(Position(1, 1))(Position(1, 2)) should be(true)

    kingThretens(Position(1, 1))(Position(2, 0)) should be(true)
    kingThretens(Position(1, 1))(Position(2, 1)) should be(true)
    kingThretens(Position(1, 1))(Position(2, 2)) should be(true)

    kingThretens(Position(1, 1))(Position(1, 3)) should be(false)

    //    countAllowedCombinations(generateCoordinates(1, 1).toSet, Nil) should be (1)
    //    countAllowedCombinations(generateCoordinates(1, 1).toSet, List(K)) should be (1)

    //    countAllowedCombinations(generateCoordinates(1, 2).toSet, List(K)) should be (2)
    //    countAllowedCombinations(generateCoordinates(2, 2).toSet, List(K, K)) should be (0)
    //
    countAllowedCombinations(generateCoordinates(2, 3), List(K, K)) should be(4)
    //    countAllowedCombinations(generateCoordinates(3, 3).toSet, List(K)) should be (9)

    countAllowedCombinations(generateCoordinates(3, 3), List(K, K)) should be(16)
    //
    countAllowedCombinations(generateCoordinates(2, 2), List(R, R)) should be(2)
    countAllowedCombinations(generateCoordinates(2, 3), List(R, R)) should be(6)

    countAllAllowedDistinctCombinations(generateCoordinates(3, 3), List(K, K, R)) should be(4)

    val startAt = System.currentTimeMillis()
    //    countAllAllowedDistinctCombinationsPar(generateCoordinates(10, 8), List(K, K, R, R, R, K)) // should be (4)
    countAllAllowedDistinctCombinations(generateCoordinates(6, 9), List(K, K, R, R, R, K)) // should be (4)
    val endAt = System.currentTimeMillis()
    println("total time (sec):" + (endAt - startAt) / 1000)
  }
}