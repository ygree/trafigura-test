package xxx.chesstask

import org.scalatest.{Matchers, FlatSpec}

class PositionMapSpec extends FlatSpec with Matchers {

  import PositionMap._

  "PositionMap of O" should "produce one safety position" in {
    val pm = O
    pm.safePositions should be (Set((0, 0)))
    pm.threatenedPositions should be (Set())
  }

  "PositionMap of one line" should "produce positions correctly" in {
    val pm = O.O.X.F
    pm.safePositions should be (Set((0, 0), (0, 1)))
    pm.threatenedPositions should be (Set((0, 2)))
    pm.figurePosition should be (0, 3)
  }

  "PositionMap of more than one line" should "produce positions correctly" in {
    val pm =
      O.X ~
      O.F
    pm.safePositions should be (Set((0, 0), (1, 0)))
    pm.threatenedPositions should be (Set((0, 1)))
    pm.figurePosition should be (1, 1)
  }
}