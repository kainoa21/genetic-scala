package zdavep.genetic

trait Genotype[T <: Gene] {
  def randomChromosome: Chromosome[T]
}
