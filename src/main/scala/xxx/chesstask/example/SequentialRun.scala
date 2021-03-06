/*
 * (c) 2014, Gribkov Yury
 * http://linkedin.com/in/ygribkov/
 */

package xxx.chesstask.example

import xxx.chesstask.impl.CombinationFinderSequential

object SequentialRun {
  def main(args: Array[String]): Unit = {
    new SimulationRunner(new CombinationFinderSequential())().run()
  }
}