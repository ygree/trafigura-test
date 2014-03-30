package xxx.chesstask

import model._
import Position._

trait Combination {
  def place(fpos: Position, f: Figure): Option[Combination]
}