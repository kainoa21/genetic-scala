package zdavep.genetic

/**
 * Top-level genetic algorithm functions
 */
object Genetic {

  /**
   * Initialize a population (search space of potential solutions).
   */
  def initPopulation[T <: Gene](initSize: Int, size: Int)(implicit
      g: Genotype[T], f: Fitness[T]): Array[Chromosome[T]] =
    (1 to initSize).map(_ => g.random).sortBy(_.fitness).take(size).toArray

  /**
   * Evolve a population for a single generation.
   */
  def evolve[T <: Gene](pop: Array[Chromosome[T]], n: Int)(implicit
    s: Selector[T], f: Fitness[T], x: Xover[T], m: Mutate[T]): Unit =
      (1 to n).foreach { _ =>
        x.crossover(s.select(pop), s.select(pop)).map(m.mutate).foreach { child =>
          val i = scala.util.Random.nextInt(pop.length)
          if (child.isMoreFit(pop(i))) {
            pop(i) = child
          }
        }
      }
}
