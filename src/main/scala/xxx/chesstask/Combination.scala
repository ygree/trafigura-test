/*
 * (c) 2014, Gribkov Yury
 * http://linkedin.com/in/ygribkov/
 */

package xxx.chesstask

import model._
import Position._

trait Combination {
  def place(fpos: Position, f: Figure): Option[Combination]
}