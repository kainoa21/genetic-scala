package zdavep
package mmc

import genetic._, Genetic._

case class Change(n: Int, coin: Coin) extends Gene {
  def value: Double = n * coin.value
  override def copy: Change = Change(n, coin)
  override def toString: String = n + " " + coin.toString
}
