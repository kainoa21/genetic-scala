package zdavep.genetic

/**
 * Search space
 */
trait Genotype[T <: Gene] {
  def randomChromosome: Chromosome[T]
}
