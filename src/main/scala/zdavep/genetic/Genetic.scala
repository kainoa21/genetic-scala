package zdavep.genetic

/**
 * Top-level genetic algorithm functions
 */
object Genetic {

  /**
   * Initialize a population (search space of potential solutions).
   */
  def initPopulation[T <: Gene](initialSize: Int, size: Int)(implicit
      g: Genotype[T], f: Fitness[T]): Array[Chromosome[T]] =
    (1 to initialSize).map(_ => g.randomChromosome).sortBy(_.fitness).take(size).toArray

  /**
   * Evolve a population for a single generation.
   */
  def evolve[T <: Gene](pop: Array[Chromosome[T]], offspring: Int)(implicit
    s: Selector[T], f: Fitness[T], x: Xover[T], m: Mutate[T]): Unit =
      (1 to offspring).foreach { _ =>
        val (p1, p2) = (s.select(pop), s.select(pop))
        x.crossover(p1, p2).foreach { child =>
          val mutated = m.mutate(child)
          val i = scala.util.Random.nextInt(pop.length)
          if (mutated.fitness < pop(i).fitness) {
            pop(i) = mutated
          }
        }
      }
}
