package zdavep.genetic

/**
 * Mutation operation
 */
trait Mutate[T <: Gene] {
  def mutate(c: Chromosome[T]): Chromosome[T]
}
