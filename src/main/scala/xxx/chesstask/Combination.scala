package xxx.chesstask

import Position._

trait Combination {
  def place(fpos: Position, f: Figure): Option[Combination]
}