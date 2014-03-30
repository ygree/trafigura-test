package xxx.chesstask.model

import Position._

trait Figure {
  def threatensAt(fpos: Position): (Position) => Boolean
}

object Figure {
  val K = King
  val Q = Queen
  val B = Bishop
  val N = Knight
  val R = Rook

  import Math.abs

  case object King extends Figure {
    def threatensAt(fpos: Position) = p => (abs(fpos._1 - p._1) < 2) && (abs(fpos._2 - p._2) < 2)
  }

  case object Queen extends Figure {
    def threatensAt(fpos: Position) = p => Bishop.threatensAt(fpos)(p) || Rook.threatensAt(fpos)(p)
  }

  case object Bishop extends Figure {
    def threatensAt(fpos: Position) = p => abs(fpos._1 - p._1) == abs(fpos._2 - p._2)
  }

  case object Knight extends Figure {
    def threatensAt(fpos: Position) = p => {
      val d1 = abs(fpos._1 - p._1)
      val d2 = abs(fpos._2 - p._2)
      (d1, d2) == (1, 2) || (d1, d2) == (2, 1)
    }
  }

  case object Rook extends Figure {
    def threatensAt(fpos: Position) = p => fpos._1 == p._1 || fpos._2 == p._2
  }
}