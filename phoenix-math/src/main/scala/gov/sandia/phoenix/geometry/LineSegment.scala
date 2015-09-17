package gov.sandia.phoenix.geometry

import scala.math._

/**
 * A line segment has a start and an end point.
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class LineSegment(start : Vector3, end : Vector3) extends IInterpolable {
  def extrapolate(t : Double) = start + (end - start) * t
  def interpolate(t : Double) = start + (end - start) * min(max(t, 0.0), 1.0)
  def length = (start - end).mag
  def mid = interpolate(0.5)

  def >(plane : Plane) = start > plane && end > plane
  def >=(plane : Plane) = start >= plane && end >= plane
  def <(plane : Plane) = start < plane && end < plane
  def <=(plane : Plane) = start <= plane && end <= plane
  def crosses(plane : Plane) = plane.dist(start) * plane.dist(end) < 0

  def closestPoint(that : LineSegment) = Linear.closestPoint(
    this.start, this.end - this.start,
    that.start, that.end - that.start) map {
    case (u, v) => (min(max(u, 0.0), 1.0), min(max(v, 0.0), 1.0))
  }
}
