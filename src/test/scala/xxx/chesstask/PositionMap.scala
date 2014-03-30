package xxx.chesstask

import xxx.chesstask.Position._
import scala.Some

class PositionMap {
  private var current: Position = (0, 0)
  private var safety: Set[Position] = Set()
  private var threatened: Set[Position] = Set()
  private var figure: Option[Position] = None

  def o: PositionMap = {
    safety += current
    incrementColumn
  }
  def X: PositionMap = {
    threatened += current
    incrementColumn
  }
  def F: PositionMap = figure match {
    case None => figure = Some(current); incrementColumn
    case Some(p) => raiseFigureAlreadyPlaced(p, current)
  }
  def ~(that: PositionMap): PositionMap = {
    (this.figure, that.figure) match {
      case (Some(f), Some(c)) => raiseFigureAlreadyPlaced(f, c)
      case (None, f)          => this.figure = f map incrementRow
      case _                  => //~
    }
    this.safety ++= that.safety map incrementRow
    this.threatened ++= that.threatened map incrementRow
    this.current = (current._1 + 1, 0)
    this
  }

  def figurePositionOpt: Option[Position] = figure

  def figurePosition: Position = figure getOrElse (throw new AssertionError("Figure has not been placed!"))

  def safePositions: Set[Position] = safety

  def threatenedPositions: Set[Position] = threatened

  private def incrementRow: Position => Position = { case (_, c) => (current._1 + 1, c) }

  private def incrementColumn: PositionMap = {
    val (row, col) = current
    current = (row, col + 1)
    this
  }

  private def raiseFigureAlreadyPlaced(figurePos: Position, currentPos: Position) =
    throw new AssertionError(s"You cannot place figure at $currentPos, because figure already placed at $figurePos.")
}

object PositionMap {
  def o: PositionMap = new PositionMap().o
  def X: PositionMap = new PositionMap().X
}