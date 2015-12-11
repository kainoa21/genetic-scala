package zdavep.mmc

sealed trait Coin {
  val value: Double
}

case object Quarter extends Coin {
  val value = 0.25D
}

case object Dime extends Coin {
  val value = 0.10D
}

case object Nickel extends Coin {
  val value = 0.05D
}

case object Penny extends Coin {
  val value = 0.01D
}
