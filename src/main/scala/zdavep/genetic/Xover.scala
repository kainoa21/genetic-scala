package zdavep.genetic

trait Xover[T <: Gene] {
  def crossover(c1: Chromosome[T], c2: Chromosome[T]): Array[Chromosome[T]]
}
