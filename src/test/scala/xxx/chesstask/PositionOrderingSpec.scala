package xxx.chesstask

import org.scalatest._

class PositionOrderingSpec extends FlatSpec with Matchers {

  "PositionsOrdering" should "be able to compare two positions" in {
    (1, 2) should be < (1, 3)
    (2, 1) should be > (1, 5)
  }
}
