package xxx.chesstask

import org.scalatest.{Matchers, FlatSpec}

class FigureSpec extends FlatSpec with Matchers {

  import Figure._
  import PositionMap._

  def checkPositions(figure: Figure, positions: PositionMap): Unit = {
    val figureAt = figure at positions.figurePosition
    println(s"figureAt: ${positions.figurePosition}")
    for (p <- positions.threatenedPositions) {
      figureAt threatens p should be (true)
    }
    for (p <- positions.safetyPositions) {
      println(p)
      figureAt threatens p should be (false)
    }
  }

  "King" should "threaten to certain positions" in {
    K at (0, 0) threatens (1, 2) should be (false)


    val pm =
    O.O.O.O.O ~
    O.X.X.X.O ~
    O.X.F.X.O ~
    O.X.X.X.O ~
    O.O.O.O.O

    checkPositions(K, pm)

//    kingThretens((0, 0))((1, 2)) should be(false)
//
//    kingThretens((1, 1))((0, 0)) should be(true)
//    kingThretens((1, 1))((0, 1)) should be(true)
//    kingThretens((1, 1))((0, 2)) should be(true)
//
//    kingThretens((1, 1))((1, 0)) should be(true)
//    //    kingThretens(Coords(1,1))(Coords(1,1)) should be (false)
//    kingThretens((1, 1))((1, 2)) should be(true)
//
//    kingThretens((1, 1))((2, 0)) should be(true)
//    kingThretens((1, 1))((2, 1)) should be(true)
//    kingThretens((1, 1))((2, 2)) should be(true)
//
//    kingThretens((1, 1))((1, 3)) should be(false)
  }
}