package gov.sandia.phoenix.eclipses

import gov.sandia.phoenix.geometry._

import scala.math._

case class Penumbra(source : Sphere, occluder : Sphere) {
  val apex = LineSegment(source.center, occluder.center).interpolate(source.radius / (source.radius + occluder.radius))
  val direction = (occluder.center - source.center).normalized
  val angle = asin((source.radius + occluder.radius) / (source.center dist occluder.center))
  val cone = InfiniteCone(apex, direction, angle)
  val clip = occluder.tangentPlane(apex).map(_.flip)

  def contains(v : Vector3) = cone.contains(v) && clip.forall(p => p <= v)
}