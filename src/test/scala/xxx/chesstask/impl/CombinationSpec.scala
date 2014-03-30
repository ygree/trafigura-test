/*
 * (c) 2014, Gribkov Yury
 * http://linkedin.com/in/ygribkov/
 */

package xxx.chesstask.impl

import org.scalatest.{Matchers, FlatSpec}
import xxx.chesstask.model.Figure

class CombinationSpec extends FlatSpec with Matchers {
  import Figure._

  "CombinationOnFunCheckOnly" should "should not allowed threatening combinations" in {
    new CombinationByFun().place((0, 0), K).get.place((0, 2), R) should be(None)
  }
}