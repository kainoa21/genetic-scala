package zdavep.genetic

import Genetic.{Fitness, Gene}

/**
 * A potential solution (ordered list of Genes).
 */
case class Chromosome[T <: Gene](genes: List[T])

/**
 * Wire fitness behavior into Chromosome.
 */
object Chromosome {
  implicit class CrossoverOps[T <: Gene](val c: Chromosome[T]) extends AnyVal {
    def fitness(implicit f: Fitness[T]): Double = f.fitness(c)
    def isMoreFit(c1: Chromosome[T])(implicit f: Fitness[T]): Boolean = f.isMoreFit(c, c1)
  }
}
