package zdavep
package tsp

import genetic._, Genetic._

case class City(name: String, lat: Double, lon: Double) extends Gene {
  override def equals(other: Any): Boolean = other match {
    case city: City => name == city.name
    case _ => false
  }
  override def hashCode: Int = name.hashCode
  override def toString: String = name
  override def copy: City = City(name, lat, lon)
}
