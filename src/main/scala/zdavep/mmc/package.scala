package zdavep

package object mmc {
  import genetic._, Genetic._
  import scala.util.Random._

  //
  private val pool = for {
    q <- 0 to 3 // max quarters
    d <- 0 to 2 // max dimes
    n <- 0 to 1 // max nickels
    p <- 0 to 4 // max pennies
  } yield List(
    Change(q, Quarter), Change(d, Dime), Change(n, Nickel), Change(p, Penny)
  )

  // Choose a random integer within a given range
  private def randInt(h: Int, l: Int = 0) = math.floor(nextDouble * (h - l)).toInt + l

  // Choose a change amount at random
  implicit val mmcGenotype: Genotype[Change] = new Genotype[Change] {
    def random: Chromosome[Change] = Chromosome(pool(randInt(pool.length)))
  }

  // Determine the fitness of an amount of change. Score is penalized if the exact target change
  // value is not found.
  implicit def mmcFitness(implicit amt: ChangeAmount): Fitness[Change] = new Fitness[Change] {
    def fitness(c: Chromosome[Change]): Double = {
      val value: Double = c.genes.foldLeft(0D)(_ + _.value)
      if (value != amt.value) 100D else c.genes.foldLeft(0)(_ + _.n).toDouble
    }
    def isMoreFit(a: Chromosome[Change], b: Chromosome[Change]): Boolean = fitness(a) < fitness(b)
  }

  // Define crossover to just generate two new change amounts at random.
  implicit val mmcXover: Xover[Change] = new Xover[Change] {
    def crossover(p0: Chromosome[Change], p1: Chromosome[Change]): Array[Chromosome[Change]] =
      Array(Chromosome(pool(randInt(pool.length))), Chromosome(pool(randInt(pool.length))))
  }

  // No mutation, just return the original amount of change
  implicit val mmcMutate: Mutate[Change] = new Mutate[Change] {
    override def mutate(c: Chromosome[Change]): Chromosome[Change] = c
  }

  // Select a chromosome at random
  implicit val mmcSelector: Selector[Change] = new Selector[Change] {
    def select(pop: Array[Chromosome[Change]]): Chromosome[Change] = pop(randInt(pop.length))
  }

  // Minimum fitness is best
  implicit def mmcOrdering(implicit f: Fitness[Change]): Ordering[Chromosome[Change]] =
    Ordering.by((c: Chromosome[Change]) => f.fitness(c))
}
