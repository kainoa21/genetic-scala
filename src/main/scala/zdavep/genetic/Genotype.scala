package zdavep.genetic

/**
 * Search space
 */
trait Genotype[T <: Gene] {
  def random: Chromosome[T]
}
