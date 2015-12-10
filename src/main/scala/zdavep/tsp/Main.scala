package zdavep
package tsp

object Main extends App {
  import genetic._, Genetic._

  implicit val tspFileReader = new TspFileReader {
    val tspFile = if (args.length >= 1) args(0).trim else "cities.tsp"
    override def readLines: List[String] = scala.io.Source.fromFile(tspFile).getLines().toList
  }

  val offspring = 100
  val size = 1000
  val initialSize = size * 2
  val maxGenerations = 2500
  val pop = initPopulation(initialSize, size)

  @scala.annotation.tailrec
  def loop(generation: Int = 1, bestFitness: Double = Double.MaxValue): Unit =
    if (generation <= maxGenerations) {
      evolve(pop, offspring)
      val currentBest = pop.min
      val currentFitness = currentBest.fitness
      if (currentFitness < bestFitness) {
        print(s"Best fitness found = $currentFitness at generation $generation\n")
        print(currentBest.genes.mkString(" -> ") + "\n\n")
        loop(generation + 1, currentFitness)
      } else {
        loop(generation + 1, bestFitness)
      }
    }
  loop()
}
