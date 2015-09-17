package gov.sandia.phoenix.geometry

import scala.math._

/**
 * Circle class.
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class Circle(center : Vector3, xAxis :  Vector3, yAxis : Vector3) extends IInterpolable {
  def radius = xAxis.mag

  @deprecated("Use sample instead", "6.4")
  def asPoints(numPoints : Int) = scala.collection.immutable.Vector.tabulate[Vector3](numPoints) { i =>
    val theta = Pi * 2.0 * i / (numPoints - 1)
    center + xAxis * cos(theta) + yAxis * sin(theta)
  }

  def interpolate(x : Double) = center + xAxis * cos(x * Pi * 2.0) + yAxis * sin(x * Pi * 2.0)

  def plane = new Plane(center, xAxis ⨯ yAxis)
}
