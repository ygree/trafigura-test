package xxx.chesstask

case class Position(m: Int, n: Int) {
  def <(that: Position): Boolean = m < that.m || m == that.m && n < that.n //TODO test
}
  
