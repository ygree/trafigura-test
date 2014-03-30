package xxx.chesstask

import org.scalatest.{Matchers, FlatSpec}

class CombinationSpec extends FlatSpec with Matchers {
  import Figure._

  "CombinationOnFunCheckOnly" should "should not allowed threatening combinations" in {
    new CombinationByFunCheckOnly().place((0, 0), K).get.place((0, 2), R) should be(None)
  }
}