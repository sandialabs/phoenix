/*
 * Copyright (c) 2016 Sandia Corporation. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by the
 * terms of this license.
 * You must not remove this notice, or any other, from this software.
 *
 * Contributors:
 * - Mark Bastian: Original author.
 * - See Git logs.
 */

package gov.sandia.phoenix.geometry

import scala.math._

/**
 * http://math.rice.edu/~pcmi/sphere/sphere.html#1
 * http://geospatialmethods.org/spheres/MiscAlgorithms.html
 * http://geospatialmethods.org/spheres/GCAIntersect.html
 * http://geospatialmethods.org/spheres/GCIntersect.html#GCIGC
 * http://www.jasondavies.com/maps/
 *
 * An arc segment on a sphere. Although not enforced at this time, these should be unit vectors.
 */
case class Arc(start : Vector3, end : Vector3) extends IInterpolable {
  def reverse = Arc(end, start)

  def plane = Plane(0, start ⨯ end)
  def contains(p : Vector3) = {
    abs(plane.dist(p)) < 1.0E-10 &&
      p.mag > 0.0 &&
      (start ⨯ p) * plane.n >= 0 &&
      (end ⨯ p) * plane.n < 0
  }

  def intersects(that : Arc) = this.plane.intersect(that.plane) match {
    case None => this.contains(that.start) || this.contains(that.end) || that.contains(this.start) || that.contains(this.end)
    case Some(ray) =>
      (this.contains(ray.direction) && that.contains(ray.direction)) ||
        (this.contains(ray.direction.negated) && that.contains(ray.direction.negated))
  }

  //Note that this plane is a great circle plane, not an arbitrary plane.
  def intersect(plane : IPlane) = this.plane.intersect(plane) flatMap { ray =>
    if(this.contains(ray.direction))
      Some(ray.direction) else if(this.contains(ray.direction.negated)) Some(ray.direction.negated) else None
  }

  //lerp.normalized might be faster and just as good.
  def interpolate(x : Double) = start.slerp(end, x)

  //
  /**
   * http://en.wikipedia.org/wiki/Axis–angle_representation
   * Plug Rodrigues' formula into (p - o) * n = 0 and solve. Group terms to get sin and cos expressions. replace sin by
   * sqrt(1 - cos * cos) and then solve the quadratic. Note that in this case omega dot v is always 0.
   * @param that
   */
  def generalIntersection(that : Plane) = {
    val v = this.start
    val n = that.n
    val omega = this.plane.n

    val A = v * n
    val ocv = omega ⨯ v
    val B = ocv * n
    val d = that.d

    val a = A * A + B * B
    val b = A * d
    val c = d * d - B * B

    val dd = b * b - a * c
    val cthetas = if(dd < 0 || a == 0) Array.empty[Double]
    else if(dd == 0) Array(b / a)
    else Array((b - sqrt(dd)) / a, (b + sqrt(dd)) / a)

    //Note that s = +/- sqrt(1 - c * c)
    val as = cthetas map { ct => v * ct + ocv * sqrt(1.0 - ct * ct) }
    val bs = cthetas map { ct => v * ct - ocv * sqrt(1.0 - ct * ct) }

    (as ++ bs) filter { p => abs(that.dist(p)) <= 1.0E-10 && this.contains(p) }
  }
}
