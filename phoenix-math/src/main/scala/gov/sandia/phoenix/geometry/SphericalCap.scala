package gov.sandia.phoenix.geometry

import scala.math._

case class SphericalCap(h : Double, r : Double) {
  //Plane distance from center of sphere
  def a = sqrt(h * (2.0 * r - h))
  def volume = Pi * h * (3.0 * a * a + h * h) / 6.0
  def area = 2.0 * Pi * r * h
}
