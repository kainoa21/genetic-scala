package zdavep.genetic

/**
 * Fitness function
 */
trait Fitness[T <: Gene] {
  def fitness(c: Chromosome[T]): Double
  def isMoreFit(a: Chromosome[T], b: Chromosome[T]): Boolean
}
