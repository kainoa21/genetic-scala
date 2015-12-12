package zdavep.mmc

sealed trait Coin {
  val value: Double
  val max: Int
}

case object Quarter extends Coin {
  val value = 0.25D
  val max = 3
}

case object Dime extends Coin {
  val value = 0.10D
  val max = 2
}

case object Nickel extends Coin {
  val value = 0.05D
  val max = 1
}

case object Penny extends Coin {
  val value = 0.01D
  val max = 4
}
