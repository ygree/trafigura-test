/*
 * (c) 2014, Gribkov Yury
 * http://linkedin.com/in/ygribkov/
 */

package xxx.chesstask.impl

import org.scalatest._
import xxx.chesstask.model.Figure

class PlacementsGeneratorSpec extends FlatSpec with Matchers {

  import Figure._

  "PlacementsGenerator" should "generateUniquePlacements" in {
    PlacementsGenerator(List()).uniquePlacements should be(Set(Nil))
    PlacementsGenerator(List(K)).uniquePlacements should be(Set(List(K)))
    PlacementsGenerator(List(K, Q)).uniquePlacements should be(Set(List(K, Q), List(Q, K)))
  }

  "PlacementsGenerator" should "be able to generate many placements" in {
    val result = PlacementsGenerator(List(N, K, K, K, K, Q, B, R, N)).uniquePlacements
    result.size should be(7560)
  }
}