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
          else Some(shouldBeSafe.mkString("Should be 'O' safe: ", ", ", ""))
        ).flatten.mkString("\n")
        MatchResult(msgs.isEmpty, msgs, "")
      } getOrElse MatchResult(false, "Figure position 'F' not specified", "")
    }
  }

  "King" should "match map" in {
    K should matchMap (
      O.O.O.O.O ~
      O.X.X.X.O ~
      O.X.F.X.O ~
      O.X.X.X.O ~
      O.O.O.O.O
    )
  }

  "Rook" should "match map" in {
    R should matchMap (
      O.O.O.O.O ~
      O.X.X.X.O ~
      O.X.F.X.O ~
      O.X.X.X.O ~
      O.O.O.O.O
    )
  }
}