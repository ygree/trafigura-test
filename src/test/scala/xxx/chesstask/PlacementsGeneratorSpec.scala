package xxx.chesstask

import org.scalatest._

class PlacementsGeneratorSpec extends FlatSpec with Matchers {

  import PlacementsGenerator._
  import Figure._

  "PlacementsGenerator" should "generateUniquePlacements" in {
    generateUniquePlacements(List()) should be(Set(Nil))
    generateUniquePlacements(List(K)) should be(Set(List(K)))
    generateUniquePlacements(List(K, Q)) should be(Set(List(K, Q), List(Q, K)))
  }

  "PlacementsGenerator" should "be able to generate many placements" in {
    val result = generateUniquePlacements(List(N, K, K, K, K, Q, B, R, N))
    result.size should be(7560)
  }
}