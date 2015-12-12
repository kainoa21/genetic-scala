package zdavep
package mmc

object Main extends App {

  implicit val changeAmount = new ChangeAmount {
    def value: Double = if (args.length >= 1) args(0).trim.toDouble else 0.41D
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
