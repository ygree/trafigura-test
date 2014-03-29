package xxx.chesstask

case class Coords(m: Int, n: Int) {
  def <(that: Coords): Boolean = m < that.m || m == that.m && n < that.n //TODO test
}
  
