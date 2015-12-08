package zdavep.genetic

trait Fitness[T <: Gene] {
  def fitness(c: Chromosome[T]): Double
}
