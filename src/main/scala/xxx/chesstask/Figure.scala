package xxx.chesstask

import Position._

trait Figure {
  def at(figurePosition: Position): FigurePlaced
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
    def at(fpos: Position) = new FigurePlaced {
      def threatens(p: Position) = (abs(fpos._1 - p._1) < 2) && (abs(fpos._2 - p._2) < 2)
    }
  }

  case object Queen extends Figure {
    def at(fpos: Position) = new FigurePlaced {
      def threatens(p: Position): Boolean = ???
    }
  }

  case object Bishop extends Figure {
    def at(fpos: Position) = new FigurePlaced {
      def threatens(p: Position): Boolean = ???
    }
  }

  case object Knight extends Figure {
    def at(fpos: Position) = new FigurePlaced {
      def threatens(p: Position): Boolean = ???
    }
  }

  case object Rook extends Figure {
    def at(fpos: Position) = new FigurePlaced {
      def threatens(p: Position) = fpos._1 == p._1 || fpos._2 == p._2
    }
  }
}