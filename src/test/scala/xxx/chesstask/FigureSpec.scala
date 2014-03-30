package xxx.chesstask

import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.matchers.{MatchResult, Matcher}

class FigureSpec extends FlatSpec with Matchers {

  import Figure._
  import PositionMap._

  def matchMap(positionMap: PositionMap) = new Matcher[Figure] {
    def apply(figure: Figure): MatchResult = {
      positionMap.figurePositionOpt map { figurePosition =>
        val figurePlaced = figure at figurePosition
        val shouldBeThreatened = for (p <- positionMap.threatenedPositions.toSeq if !(figurePlaced threatens p)) yield p
        val shouldBeSafe = for (p <- positionMap.safePositions.toSeq if figurePlaced threatens p) yield p
        val msgs = Seq(
          if (shouldBeThreatened.isEmpty) None
          else Some(shouldBeThreatened.mkString("Should be 'X' threatened: ", ", ", "")),
          if (shouldBeSafe.isEmpty) None
          else Some(shouldBeSafe.mkString("Should be 'o' safe: ", ", ", ""))
        ).flatten.mkString("\n")
        MatchResult(msgs.isEmpty, msgs, "")
      } getOrElse MatchResult(false, "Figure position 'F' not specified", "")
    }
  }

  "King" should "match map" in {
    King should matchMap (
      o.o.o.o.o ~
      o.X.X.X.o ~
      o.X.F.X.o ~
      o.X.X.X.o ~
      o.o.o.o.o
    )
  }

  "Queen" should "match map" in {
    Queen should matchMap (
      X.o.X.o.X ~
      o.X.X.X.o ~
      X.X.F.X.X ~
      o.X.X.X.o ~
      X.o.X.o.X
    )
  }

  "Bishop" should "match map" in {
    Bishop should matchMap (
      X.o.o.o.X ~
      o.X.o.X.o ~
      o.o.F.o.o ~
      o.X.o.X.o ~
      X.o.o.o.X
    )
  }

  "Rook" should "match map" in {
    Rook should matchMap (
      o.o.X.o.o ~
      o.o.X.o.o ~
      X.X.F.X.X ~
      o.o.X.o.o ~
      o.o.X.o.o
    )
  }
}