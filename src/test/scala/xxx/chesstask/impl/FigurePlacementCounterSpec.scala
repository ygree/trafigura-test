/*
 * (c) 2014, Gribkov Yury
 * http://linkedin.com/in/ygribkov/
 */

package xxx.chesstask.impl

import org.scalatest.{Matchers, FlatSpec}
import xxx.chesstask.model.{Position, Figure}

class FigurePlacementCounterSpec extends FlatSpec with Matchers {
  import Figure._
  import Position._

  "FigurePlacementCounter" should "count all valid unique combinations" in {
    val finder = new FigurePlacementCounter()
    import finder._

    find(allPositionsOrdered(2, 3), List(K, K)) should be(4)
    find(allPositionsOrdered(3, 3), List(K, K)) should be(16)
    find(allPositionsOrdered(2, 2), List(R, R)) should be(2)
    find(allPositionsOrdered(2, 3), List(R, R)) should be(6)
  }
}