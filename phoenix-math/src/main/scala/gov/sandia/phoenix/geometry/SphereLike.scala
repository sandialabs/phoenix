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

import gov.sandia.phoenix.math._

import scala.math._

trait SphereLike extends RayIntersection with LineSegmentIntersectable {
  def center : Vector3
  def radius : Double
  
  val area = 4.0 * Pi * radius * radius
  val volume = 4.0 * Pi * radius * radius * radius / 3.0
  
  def normal(p : Vector3) = (p - center).normalized
  
  def intersects(ls : LineSegment) : Boolean

  val apparentRadius = { o : Vector3 =>
    val r = center dist o
    if(r < radius) None else Some { radius / r * sqrt(r * r - radius * radius) }
  }

  /**
   * We can eliminate a from the quadratic because the direction is a unit
   * vector.  We can remove the 2s and 4s because a convenient 2 pops out of
   * the ray-sphere parametric equation on the b term that does some convenent
   * cancellations.
   * http://www.siggraph.org/education/materials/HyperGraph/raytrace/rtinter0.htm
   * http://wiki.cgsociety.org/index.php/Ray_Sphere_Intersection
   */
  def intersect(ray : Ray) : Array[Double] = {
    val omc = ray.origin - center
    val b = omc * ray.direction
    val c = omc * omc - radius * radius
    val des = b * b - c
      
    if(des > 0) Array(-b - math.sqrt(des), -b + math.sqrt(des)) else
      if(des == 0.0) Array(-b) else new Array[Double](0)
  }
  
  /**
   * http://www.siggraph.org/education/materials/HyperGraph/raytrace/rtinter0.htm
   * http://wiki.cgsociety.org/index.php/Ray_Sphere_Intersection
   */
  def intersect(ls : LineSegment) : Array[Double] = {
    val omc = ls.start - center
    val v = ls.end-ls.start
    val a = v*v
    val b = omc * v
    val c = omc * omc - radius * radius
    val des = b * b - a * c
      
    if(des > 0) {
      Array((-b - math.sqrt(des)) / a, (-b + math.sqrt(des)) / a)
    } else if(des == 0.0) Array(-b) else Array.ofDim[Double](0)
  }

  val tangentRange = { p : Vector3 =>
    val x = (p - center).magSquared - radius * radius
    if(x >= 0) Some(sqrt(x)) else None
  }

  val tangentAngle = { p : Vector3 =>
    val R = (p - center).mag
    if(R >= radius) Some(casin(radius / R)) else None
  }

  val tangentPlane = { p : Vector3 =>
    val lookAt = p - center
    val R = lookAt.mag
    val d = radius * radius / R
    if(d > radius) None else Some {
      val n = lookAt.normalized
      val o = center + n * d
      new Plane(o, n)
    }
  }

  val tangentCircle = { p : Vector3 =>
    val lookAt = p - center
    val R = lookAt.mag
    val d = radius * radius / R
    if(d > radius) None else Some {
      val n = lookAt.normalized
      val o = center + n * d
      val r = sqrt(radius * radius - d * d)
      val x = ~n
      val y = n ⨯ x
      new Circle(o, x * r, y * r)
    }
  }

  val cut = { p : Plane =>
    val d = abs(p.d)
    if(d >= 0 && d < radius)
      Some(SphericalCap(radius - d, radius))
    else None
  }
  
  val intersect = { p : Plane =>
    val d = abs(p.dist(center))
    if(d > radius) null else {
      val r = sqrt(radius * radius - d * d)
      val x = X_AXIS * r
      val y = Y_AXIS * r
      val t = !p.n.rotInto(Z_AXIS)
      new Circle(t * new Vector3(0, 0, d) + center, t * x, t * y)
    }
  }
  
  /**
   * Compute the point of reflection between two points.  This point will be 
   * the one at which the equation (p0 - pReflect) * n = (p1 - pReflect) * n,
   * where n is the outward facing surface normal at the point pReflect.
   * @param p0
   * @param p1
   * @return the reflection point on the sphere.
   * 
   */
  def reflect(p0 : Vector3, p1 : Vector3) = {
    //overhaul the lines below to use the new boolean intersects function
    val ray = Ray.fromPoints(p0, p1)
    if(intersect(new LineSegment(p0, p1)).length != 0) null else {
      val u = (p0 - center).normalized
      val v = (p1 - center).normalized
        
      val n = u % v
      val cosTheta = u * v
      val sinTheta = n.mag
      val theta = atan2(sinTheta, cosTheta)
      val s = sin(theta)
        
      var t0 = 0.0
      var t1 = 1.0
      var ft0 = f(t0, p0, p1, u, v, theta, s)
      var ft1 = f(t1, p0, p1, u, v, theta, s)
      var t2 = t1 - ft1 * (t1 - t0) / (ft1 - ft0)
      var ft2 = f(t2, p0, p1, u, v, theta, s)
        
      while(abs(ft2) > 1E-6) {
        t0 = t1
        t1 = t2
        ft0 = ft1
        ft1 = ft2
        t2 = t1 - ft1 * (t1 - t0) / (ft1 - ft0)
        ft2 = f(t2, p0, p1, u, v, theta, s)
      }
        
      (u * sin((1.0 - t2) * theta) + v * sin(t2 * theta)) * radius / s
    }
  }
  
  private def f(t : Double, p0 : Vector3, p1 : Vector3, u : Vector3, v : Vector3, theta : Double, s : Double) : Double = {
    val pp = (u * sin((1.0 - t) * theta) + v * sin(t * theta)) * radius / s
    val n = pp.normalized      
    val dhi = (p0 - pp).normalized * n
    val dlo = (p1 - pp).normalized * n
    dhi - dlo
  }
}
