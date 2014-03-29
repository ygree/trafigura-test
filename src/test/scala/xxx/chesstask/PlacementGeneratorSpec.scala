package xxx.chesstask

import org.scalatest._

class PlacementGeneratorSpec extends FlatSpec with Matchers {

  import PlacementsGenerator._
  import Figure._

  "PlacementGenerator" should "generateUniquePlacements" in {
    generateUniquePlacements(List()) should be (Set(Nil))
    generateUniquePlacements(List(K)) should be (Set(List(K)))
    generateUniquePlacements(List(K,Q)) should be (Set(List(K,Q), List(Q,K)))
  }
  
  "PlacementGenerator" should "be albe to generate many combinations" in {
    val result = generateUniquePlacements(List(N,K,K,K,K,Q,B,R,N))
    result.size should be (360)
  }
}