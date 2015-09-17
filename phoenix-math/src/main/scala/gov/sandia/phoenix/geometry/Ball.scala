package gov.sandia.phoenix.geometry

/**
 * A representation of a Sphere.
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class Ball(center : Vector3, radius : Double)
extends SphereLike with Solid {
  def this() = this(ORIGIN, 1.0)
  def this(r : Double) = this(ORIGIN, r)
  
  /**
   * This can be inconsistent with intersect due to roundoff. Perhaps the best
   * answer is to compare it with the number of results rather than this calc.
   */
  override def intersects(ls : LineSegment) = center.dist(ls) <= radius
  override def toString = "r = " + radius + ", c = " + center
}
