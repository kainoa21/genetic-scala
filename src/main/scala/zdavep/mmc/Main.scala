package zdavep
package mmc

object Main extends App {
  import genetic._, Genetic._

  implicit val changeAmount = new ChangeAmount {
    def value: Double = 0.41D
  }

  val size = 1000
  val pop = initPopulation(size * 2, size)

  evolve(pop)
  val best = pop.min
  val total = best.genes.foldLeft(0D)(_ + _.value)

  assert(changeAmount.value == total)
  print(s"Solution = ${best.fitness.toInt} coins; " + best.genes.mkString(", ") +
    " = $" + total + "\n")
}
