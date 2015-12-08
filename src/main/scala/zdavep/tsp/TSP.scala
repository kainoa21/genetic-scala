package zdavep
package tsp

object TSP extends App {
  import genetic._, Genetic._

  val offspring = 100
  val size = 1000
  val initialSize = size * 2
  val maxGenerations = 2500
  val pop = initPopulation(initialSize, size)

  @scala.annotation.tailrec
  def iterate(generation: Int = 1, bestFitness: Double = Double.MaxValue): Unit =
    if (generation <= maxGenerations) {
      evolve(pop, offspring)
      val currentBest = pop.min
      val currentFitness = currentBest.fitness
      if (currentFitness < bestFitness) {
        print(s"Best fitness found = $currentFitness at generation $generation\n")
        print(currentBest.genes.mkString(" -> ") + "\n\n")
        iterate(generation + 1, currentFitness)
      } else {
        iterate(generation + 1, bestFitness)
      }
    }

  iterate()

}
