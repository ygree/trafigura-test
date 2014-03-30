package xxx.chesstask.impl

import CombinationByFun._
import xxx.chesstask.model.{Figure, Position}
import Position._
import scala.Some
import xxx.chesstask.Combination

class CombinationByFun(
  allowedPositionFun: CheckPosition = alwaysTrue,
  takenPositions: List[Position] = Nil
) extends Combination
{
  import CombinationByFun._

  def place(fpos: Position, f: Figure): Option[Combination] =
    if (!allowedPositionFun(fpos) || (takenPositions exists threatenedPositionFunWithFallback(f, fpos))) None else {
      val newFun = safetyPositionFun(f, fpos) orElse allowedPositionFun
      Some(new CombinationByFun(newFun, fpos :: takenPositions))
    }
}

object CombinationByFun {

  type CheckPosition = PartialFunction[Position, Boolean]

  val alwaysTrue: CheckPosition = {
    case _ => true
  }

  def threatenedPositionFunWithFallback(f: Figure, fpos: Position): CheckPosition =
    not (safetyPositionFun(f, fpos) orElse alwaysTrue)

  def safetyPositionFun(f: Figure, fpos: Position): CheckPosition = {
    case p if (f threatensAt fpos)(p) => false
  }

  private def not(f: CheckPosition): CheckPosition = {
    case p => !f(p)
  }
}