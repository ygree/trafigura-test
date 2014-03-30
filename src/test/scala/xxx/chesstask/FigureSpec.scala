package xxx.chesstask

import org.scalatest.{Matchers, FlatSpec}

class FigureSpec extends FlatSpec with Matchers {

  import Figure._
  import PositionMap._

  "King" should "match map" in {
    King should matchMap (
      o.o.o.o.o ~
      o.X.X.X.o ~
      o.X.F.X.o ~
      o.X.X.X.o ~
      o.o.o.o.o
    )
  }

  "Queen" should "match map" in {
    Queen should matchMap (
      X.o.X.o.X ~
      o.X.X.X.o ~
      X.X.F.X.X ~
      o.X.X.X.o ~
      X.o.X.o.X
    )
  }

  "Bishop" should "match map" in {
    Bishop should matchMap (
      X.o.o.o.X ~
      o.X.o.X.o ~
      o.o.F.o.o ~
      o.X.o.X.o ~
      X.o.o.o.X
    )
  }

  "Knight" should "match map" in {
    Knight should matchMap (
      o.X.o.X.o ~
      X.o.o.o.X ~
      o.o.F.o.o ~
      X.o.o.o.X ~
      o.X.o.X.o
    )
  }

  "Rook" should "match map" in {
    Rook should matchMap (
      o.o.X.o.o ~
      o.o.X.o.o ~
      X.X.F.X.X ~
      o.o.X.o.o ~
      o.o.X.o.o
    )
  }
}