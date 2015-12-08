package zdavep.genetic

trait Mutate[T <: Gene] {
  def mutate(c: Chromosome[T]): Chromosome[T]
}
