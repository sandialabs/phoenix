package gov.sandia.phoenix.geometry

import scala.math._


/**
 * A representation of a cone that extends forever.
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class InfiniteCone(point : Vector3, dir : Vector3, theta : Double) {
  val direction = dir.normalized
  def contains(v : Vector3) = angularSeparation(v) <= theta
  def angularSeparation(v : Vector3) = acos(cosAngularSeparation(v))
  def cosAngularSeparation(v : Vector3) = min(max((v - point).normalized * direction, -1.0), 1.0)
  
  def intersect(sphere : Sphere) = for {
    d <- direction.sweep(theta, 100)
    intersection = new Ray(point, d)->sphere
    if intersection != null
      } yield intersection
  }
