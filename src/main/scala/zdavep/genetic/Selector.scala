package zdavep.genetic

/**
 * Selection operator
 */
trait Selector[T <: Gene] {
  def select(pop: Array[Chromosome[T]]): Chromosome[T]
}
