package zdavep.genetic

/**
 * Top-level genetic algorithm types
 */
object Genetic {

  /**
   * The basic unit of a genetic algorithm.
   */
  trait Gene { self =>
    def copy: Gene = self
  }

  /**
   * Fitness function
   */
  trait Fitness[T <: Gene] {
    def fitness(c: Chromosome[T]): Double
    def isMoreFit(a: Chromosome[T], b: Chromosome[T]): Boolean
  }

  /**
   * Search space
   */
  trait Genotype[T <: Gene] {
    def random: Chromosome[T]
  }

  /**
   * Mutation operation
   */
  trait Mutate[T <: Gene] {
    def mutate(c: Chromosome[T]): Chromosome[T]
  }

  /**
   * Selection operator
   */
  trait Selector[T <: Gene] {
    def select(pop: Array[Chromosome[T]]): Chromosome[T]
  }

  /**
   * Crossover operation
   */
  trait Xover[T <: Gene] {
    def crossover(c1: Chromosome[T], c2: Chromosome[T]): Array[Chromosome[T]]
  }
}
