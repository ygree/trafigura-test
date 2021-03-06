/*
 * (c) 2014, Gribkov Yury
 * http://linkedin.com/in/ygribkov/
 */

package xxx.chesstask.model

import org.scalatest.{Matchers, FlatSpec}

class PositionMapSpec extends FlatSpec with Matchers {

  import PositionMap._

  "PositionMap of one 'o' element" should "produce one safety position" in {
    val pm = o
    pm.safePositions should be (Set((0, 0)))
    pm.threatenedPositions should be (Set())
  }

  "PositionMap of one line" should "produce positions correctly" in {
    val pm = o.o.X.F
    pm.safePositions should be (Set((0, 0), (0, 1)))
    pm.threatenedPositions should be (Set((0, 2)))
    pm.figurePosition.get should be (0, 3)
  }

  "PositionMap of more than one line" should "produce positions correctly" in {
    val pm =
      o.X ~
      o.F
    pm.safePositions should be (Set((0, 0), (1, 0)))
    pm.threatenedPositions should be (Set((0, 1)))
    pm.figurePosition.get should be (1, 1)
  }
}