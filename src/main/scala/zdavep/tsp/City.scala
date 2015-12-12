package zdavep
package tsp

import genetic._, Genetic._

case class City(name: String, lat: Double, lon: Double) extends Gene {
  override def toString: String = name
  override def copy: City = City(name, lat, lon)
}
