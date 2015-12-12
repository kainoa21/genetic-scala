package zdavep
package mmc

object Main extends App {

  implicit val changeAmount = new ChangeAmount {
    @scala.annotation.tailrec
    def loop(d: Double): Double = if (d > 1.0) loop(d - 1.0) else round(d)
    def value: Double = loop(if (args.length >= 1) args(0).trim.toDouble else 0.41D)
  }

  val size = 1000
  val pop = genetic.initPopulation(size * 2, size)

  genetic.evolve(pop)
  val best = pop.min
  val total = round(best.genes.foldLeft(0D)(_ + _.value))

  assert(changeAmount.value == total)
  print(s"Solution = ${best.fitness.toInt} coins; " + best.genes.mkString(", ") +
    " = $" + total + "\n")
}
