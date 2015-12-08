package zdavep

/**
 * TSP-specific genetic algorithm values and functions
 */
package object tsp {
  import scala.annotation.tailrec
  import scala.util.Random.nextDouble
  import zdavep.genetic._

  // The radius of earth in miles
  private final val RADIUS_EARTH = 3958.761

  // GCD calculation helper constant
  private final val PI_RADS_PER_180_DEG = math.Pi / 180D

  // TSP crossover rate
  private final val XOVER_RATE  = 0.9D

  // TSP mutation rate
  private final val MUTATE_RATE = 0.1D

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
    list.map(c => (distance(city, c), c)).sortBy(_._1).head._2

  // Nearest neighbor optimization function
  @tailrec
  private def nearestNeighbor(city: City, src: List[City], dest: List[City]): Chromosome[City] =
    if (src.isEmpty) Chromosome(dest :+ city) else {
      val nearest = findNearest(city, src)
      nearestNeighbor(nearest, src.filterNot(_.equals(nearest)), dest :+ city)
    }

  // TSP genotype - represents the search space of possible tours (lists of cities)
  implicit def tspGenotype: Genotype[City] = new Genotype[City] {
    import scala.io.Source.fromFile
    import scala.util.Random.shuffle

    private val genePool = readFile("cities.tsp")

    private def readFile(file: String): List[City] = fromFile(file).getLines().map(parseCity).toList

    private def parseCity(line: String): City = {
      val Array(name, lat, lon) = line.split("\\s")
      City(name.trim, lat.toDouble, lon.toDouble)
    }

    override def randomChromosome: Chromosome[City] = shuffle(genePool.map(_.copy)) match {
      case h :: t => nearestNeighbor(h, t, Nil)
      case Nil => Chromosome(genePool)
    }
  }

  // TSP fitness - uses the great circle distance function to calculate the length of a tour
  implicit def tspFitness: Fitness[City] = new Fitness[City] {
    @tailrec
    final def calculate(city: City, cities: List[City], dist: Double): Double = cities match {
      case h :: t => calculate(h, t, dist + distance(city, h))
      case Nil => dist
    }
    override def fitness(c: Chromosome[City]): Double =
      calculate(c.genes.head, c.genes.tail, distance(c.genes.last, c.genes.head))
  }

  // Choose a random integer within a given range
  private def randInt(h: Int, l: Int = 0) = math.floor(nextDouble * (h - l + 1)).toInt + l

  // TSP crossover - split two chromosomes at a single index and cross combine
  implicit def singlePointXover[T <: Gene]: Xover[T] = new Xover[T] {
    def crossover(p0: Chromosome[T], p1: Chromosome[T]): Array[Chromosome[T]] =
      if (nextDouble > XOVER_RATE) Array() else {
        val len = p0.genes.length
        val c0 = p0.genes.splitAt(randInt(len * 2 / 3, len / 3))._1
        val c1 = p1.genes diff c0
        Array(Chromosome(c0 ++ c1), Chromosome(c1 ++ c0))
      }
  }

  // TSP mutation - reverse an internal slice of a tour
  implicit def reverseMutator[T <: Gene]: Mutate[T] = new Mutate[T] {
    def mutate(c: Chromosome[T]): Chromosome[T] = if (nextDouble > MUTATE_RATE) c else {
      val l1 = c.genes.length
      val l2 = l1 / 2
      val (i1, i2) = (randInt(l2), randInt(l1, l2))
      Chromosome(c.genes.take(i1) ++ c.genes.slice(i1, i2).reverse ++ c.genes.drop(i2))
    }
  }

  // TSP selection - select a chromosome at random
  implicit def randomSelector[T <: Gene]: Selector[T] = new Selector[T] {
    def select(pop: Array[Chromosome[T]]): Chromosome[T] = pop(randInt(pop.length - 1))
  }
}
