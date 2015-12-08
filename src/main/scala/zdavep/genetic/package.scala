package zdavep

package object genetic {
  implicit class PopulationOps[T <: Gene](val pop: Array[Chromosome[T]]) extends AnyVal {
    def minByFitness(implicit f: Fitness[T]): Chromosome[T] = pop.minBy(f.fitness)
    def maxByFitness(implicit f: Fitness[T]): Chromosome[T] = pop.maxBy(f.fitness)
  }
}
