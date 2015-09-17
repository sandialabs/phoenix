package gov.sandia.phoenix.geometry

import scala.math._

case class Torus(R : Double, r : Double) {
  def dist(p : Vector3) = {
    val a = R - hypot(p.x, p.y)
    sqrt(a * a + p.z * p.z) - r
  }

  def contains(p : Vector3) = dist(p) <= 0

  def area = 4.0 * Pi * Pi * R * r
  def volume = 2.0 * Pi * Pi * R * r * r
}