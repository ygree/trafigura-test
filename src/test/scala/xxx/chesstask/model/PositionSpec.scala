package xxx.chesstask.model

import org.scalatest._
import Position._

class PositionSpec extends FlatSpec with Matchers {

  "Positions" should "be able comparable" in {
    (1, 2) < (1, 3) should be (true)
    (2, 1) < (1, 5) should be (false)
  }

  "Position" should "generate all possible unique ordered positions for specified dimentions" in {
    val result = allPositionsOrdered(6, 9)
    result.size should be (6 * 9)
    result.size should be (result.toSet.size)
    result.toSet.size should be (result.size)
    //TODO check ordering
  }
}