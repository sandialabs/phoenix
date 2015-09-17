package gov.sandia.phoenix.geometry

import scala.math._

/**
 * A cone class.
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class Cone(point : Vector3, direction : Vector3, radius : Double, height : Double) {
  def volume = Pi * radius * radius * height / 3.0
}
