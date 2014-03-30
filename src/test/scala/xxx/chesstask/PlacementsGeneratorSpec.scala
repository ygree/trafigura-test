package xxx.chesstask

import org.scalatest._

class PlacementsGeneratorSpec extends FlatSpec with Matchers {

  import Figure._

  "PlacementsGenerator" should "generateUniquePlacements" in {
    PlacementsGenerator(List()).generateUniquePlacements should be(Set(Nil))
    PlacementsGenerator(List(K)).generateUniquePlacements should be(Set(List(K)))
    PlacementsGenerator(List(K, Q)).generateUniquePlacements should be(Set(List(K, Q), List(Q, K)))
  }

  "PlacementsGenerator" should "be able to generate many placements" in {
    val result = PlacementsGenerator(List(N, K, K, K, K, Q, B, R, N)).generateUniquePlacements
    result.size should be(7560)
  }
}