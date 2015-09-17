package gov.sandia.phoenix.geometry

import scala.math._

/**
 * @see http://mathworld.wolfram.com/SphericalCoordinates.html
 * 
 * @author <A HREF="mailto:markbastian@gmail.com">Mark Bastian</A>
 *
 */
case class SphericalCoordinate(r : Double, theta : Double, phi : Double) {
  def toCartesian = new Vector3(r * cos(theta) * sin(phi), r * sin(theta) * sin(phi), r * cos(phi))
}