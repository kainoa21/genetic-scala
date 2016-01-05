package zdavep
package mmc

/**
 * Minimize make change example.
 */
object Main extends App {

  // Read target change amount from CLI. Default to $0.41 if not provided.
  implicit val changeAmount = new ChangeAmount {
    @scala.annotation.tailrec
    def loop(d: Double): Double = if (d > 1.0) loop(d - 1.0) else round(d)
    def value: Double = loop(if (args.length >= 1) args(0).trim.toDouble else 0.41D)
  }

  // Init population
  val size = 1000
  val pop = genetic.init(size * 2, size)

  // Only need to evolve once
  genetic.evolve(pop)
  val best = pop.min
  val total = round(best.foldLeft(0D)(_ + _.value))

  // Print the optimal amount of change
  assert(changeAmount.value == total)
  print(s"Solution = ${best.foldLeft(0)(_ + _.n)} coins; " + best.mkString(", ") + " = $" + total + "\n")
}
