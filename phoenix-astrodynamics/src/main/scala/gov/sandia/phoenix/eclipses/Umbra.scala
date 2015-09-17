package gov.sandia.phoenix.eclipses

import gov.sandia.phoenix.geometry.{InfiniteCone, LineSegment, Sphere, Vector3}

import scala.math._

case class Umbra(source : Sphere, occluder : Sphere) {
  val apex = LineSegment(source.center, occluder.center).extrapolate(1.0 + occluder.radius / (source.radius - occluder.radius))
  val direction = (source.center - occluder.center).normalized
  val angle = asin((source.radius - occluder.radius) / (source.center dist occluder.center))
  val cone = InfiniteCone(apex, direction, angle)
  val clip = occluder.tangentPlane(apex)

  def contains(v : Vector3) = cone.contains(v) && clip.forall(p => p <= v)
}