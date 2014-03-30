package xxx.chesstask

import Position._

trait Figure {
  def threatensFun(fpos: Position): (Position) => Boolean
  def at(fpos: Position) = new FigurePlaced {
    def threatens(p: Position) = threatensFun(fpos)(p)
  }
}

trait FigurePlaced {
  def threatens(checkingPosition: Position): Boolean
}

object Figure {

  val K = King
  val Q = Queen
  val B = Bishop
  val N = Knight
  val R = Rook

  import Math.abs

  case object King extends Figure {
    def threatensFun(fpos: Position) = p => (abs(fpos._1 - p._1) < 2) && (abs(fpos._2 - p._2) < 2)
  }

  case object Queen extends Figure {
    def threatensFun(fpos: Position) = p => Bishop.threatensFun(fpos)(p) || Rook.threatensFun(fpos)(p)
  }

  case object Bishop extends Figure {
    def threatensFun(fpos: Position) = p => abs(fpos._1 - p._1) == abs(fpos._2 - p._2)
  }

  case object Knight extends Figure {
    def threatensFun(fpos: Position) = ???
  }

  case object Rook extends Figure {
    def threatensFun(fpos: Position) = p => fpos._1 == p._1 || fpos._2 == p._2
  }
}