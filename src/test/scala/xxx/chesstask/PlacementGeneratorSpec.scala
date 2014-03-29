package xxx.chesstask

import org.scalatest._

class PlacementGeneratorSpec extends FlatSpec with Matchers {

  import PlacementsGenerator._
  import Figure._

  "PlacementGenerator" should "excludeFollowingDups" in {
    excludeFollowingDups(Nil) should be (Nil)
    excludeFollowingDups(List(K)) should be (List(K))
    excludeFollowingDups(List(K,B,B,Q,K)) should be (List(K,B,Q))
  }

  "PlacementGenerator" should "generateUniquePlacements" in {
//    generateUniquePlacements(Array()) should be (List(Nil))
    generateUniquePlacements(Array(K)) should be (List(List(K)))
    generateUniquePlacements(Array(K,K)) should be (List(List(K,K)))
    generateUniquePlacements(Array(K,Q)) should be (List(List(K,Q), List(Q,K)))
  }

}