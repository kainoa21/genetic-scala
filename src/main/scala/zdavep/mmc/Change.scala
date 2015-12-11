package zdavep.mmc

case class Change(n: Int, coin: Coin) extends zdavep.genetic.Gene {
  def value: Double = n * coin.value
  override def copy: Change = Change(n, coin)
  override def toString: String = n + " " + coin.toString
}
