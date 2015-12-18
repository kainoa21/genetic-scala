package zdavep

/**
 * TSP-specific genetic algorithm values and functions
 */
package object tsp {
  import genetic._, Genetic._
  import scala.annotation.tailrec
  import scala.util.Random.nextDouble

  // The radius of earth in miles
  private final val RADIUS_EARTH = 3958.761

  // GCD calculation helper constant
  private final val PI_RADS_PER_180_DEG = math.Pi / 180D

  // TSP crossover rate
  private final val XOVER_RATE = 0.9D

  // TSP mutation rate
  private final val MUTATE_RATE = 0.1D

  // Choose a random integer within a given range
  private def randInt(h: Int, l: Int = 0) = math.floor(nextDouble * (h - l)).toInt + l

  // Great circle distance function (assumes earth is a sphere)
  def distance(c1: City, c2: City): Double = {
    val p1 = c1.lat * PI_RADS_PER_180_DEG
    val p2 = c2.lat * PI_RADS_PER_180_DEG
    val p3 = c2.lon * PI_RADS_PER_180_DEG - c1.lon * PI_RADS_PER_180_DEG
    val p4 = math.sin(p1) * math.sin(p2)
    val p5 = math.cos(p1) * math.cos(p2) * math.cos(p3)
    RADIUS_EARTH * math.acos(p4 + p5)
  }

  // Given a city, find it's nearest neighbor
  private def findNearest(city: City, list: List[City]): City =
    list.map(c => (distance(city, c), c)).minBy(_._1)._2

  // Nearest neighbor optimization function
  @tailrec
  private def nearestNeighbor(c: City, src: List[City], dest: List[City] = Nil): Chromosome[City] =
    if (src.isEmpty) Chromosome(dest :+ c) else {
      val nearest = findNearest(c, src)
      nearestNeighbor(nearest, src.filterNot(_ == nearest), dest :+ c)
    }

  // TSP genotype - represents the search space of possible tours (lists of cities)
  implicit def tspGenotype(implicit r: TspFileReader): Genotype[City] = new Genotype[City] {
    import scala.util.Random.shuffle

    private val genePool = r.readLines.map { line =>
      val Array(name, lat, lon) = line.split("\\s")
      City(name.trim, lat.toDouble, lon.toDouble)
    }

    def random: Chromosome[City] = shuffle(genePool.map(_.copy)) match {
      case h :: t => nearestNeighbor(h, t)
      case Nil => Chromosome(genePool)
    }
  }

  // TSP fitness - uses the great circle distance function to calculate the length of a tour
  implicit val tspFitness: Fitness[City] = new Fitness[City] {
    @tailrec private def calc(cities: List[City], acc: Double): Double = cities match {
      case h :: t => if (t.isEmpty) acc else calc(t, acc + distance(h, t.head))
      case Nil => acc
    }
    def fitness(c: Chromosome[City]): Double = calc(c.genes, distance(c.genes.last, c.genes.head))
    def isMoreFit(a: Chromosome[City], b: Chromosome[City]): Boolean = fitness(a) < fitness(b)
  }

  // TSP crossover - split two chromosomes at a single index and cross combine
  implicit val tspXover: Xover[City] = new Xover[City] {
    def crossover(p0: Chromosome[City], p1: Chromosome[City]): Array[Chromosome[City]] =
      if (nextDouble > XOVER_RATE) Array.empty[Chromosome[City]] else {
        val len = p0.genes.length
        val c0 = p0.genes.splitAt(randInt(len * 2 / 3, len / 3))._1
        val c1 = p1.genes diff c0
        Array(Chromosome(c0 ++ c1), Chromosome(c1 ++ c0))
      }
  }

  // TSP mutation - reverse an internal slice of a tour
  implicit val tspMutate: Mutate[City] = new Mutate[City] {
    def mutate(c: Chromosome[City]): Chromosome[City] = if (nextDouble > MUTATE_RATE) c else {
      val l1 = c.genes.length
      val l2 = l1 / 2
      val (i1, i2) = (randInt(l2), randInt(l1, l2))
      Chromosome(c.genes.take(i1) ++ c.genes.slice(i1, i2).reverse ++ c.genes.drop(i2))
    }
  }

  // TSP selection - select a chromosome at random
  implicit val tspSelector: Selector[City] = new Selector[City] {
    def select(pop: Array[Chromosome[City]]): Chromosome[City] = pop(randInt(pop.length))
  }

  // TSP ordering - minimum fitness is best.
  implicit def tspOrdering(implicit f: Fitness[City]): Ordering[Chromosome[City]] =
    Ordering.by((c: Chromosome[City]) => f.fitness(c))
}
