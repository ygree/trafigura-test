package xxx.chesstask

import xxx.chesstask.Position._
import scala.Some
import xxx.chesstask.Combination.CheckPosition

object Combination {
  private type CheckPosition = PartialFunction[Position, Boolean]

  def apply(): Combination = new Combination(alwaysTrue)

  private val alwaysTrue: CheckPosition = {
    case _ => true
  }
}

class Combination private(checkIfAllowedPosition: CheckPosition, pos: List[Position] = Nil, figures: List[Figure] = Nil) {
  def place(c: Position, f: Figure): Option[Combination] =
    if (!checkIfAllowedPosition(c)) {
      None
    }
    else if (pos.exists(p => !(isSafetyPositionFun(f, c) orElse Combination.alwaysTrue)(p))) None
    else {
      val newFun = isSafetyPositionFun(f, c) orElse checkIfAllowedPosition
      Some(new Combination(newFun, c :: pos, f :: figures))
    }

  def print() = println("found combination " + pos.reverse + " for " + figures.reverse)

  def isSafetyPositionFun(f: Figure, fpos: Position): CheckPosition = {
    case p if (f threatensAt fpos)(p) => false
  }
}