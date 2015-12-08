package zdavep.genetic

/**
 * Selection operator
 */
trait Selector[T <: Gene] {
  def select(pop: Array[Chromosome[T]]): Chromosome[T]
  def selectTwo(pop: Array[Chromosome[T]]): (Chromosome[T], Chromosome[T]) =
    (select(pop), select(pop))
}
