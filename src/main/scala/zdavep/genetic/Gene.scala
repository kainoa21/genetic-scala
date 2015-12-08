package zdavep.genetic

/**
 * The basic unit of a genetic algorithm.
 */
trait Gene { self =>
  def copy: Gene = self
}
