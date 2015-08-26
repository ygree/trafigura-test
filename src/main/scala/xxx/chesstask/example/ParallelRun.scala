/*
 * (c) 2014, Gribkov Yury
 * http://linkedin.com/in/ygribkov/
 */

package xxx.chesstask.example

import xxx.chesstask.impl.CombinationFinderOnFutures

object ParallelRun {
  def main(args: Array[String]): Unit = {
    new SimulationRunner(new CombinationFinderOnFutures())().run()
  }
}