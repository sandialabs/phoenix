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
 * A ray is a point and a direction. The direction can be thought of as a line
 * that extends to infinity in that direction.
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class Ray(origin : Vector3, unscaledDirection : Vector3) {
  //For Java compatibility
  def intersect(o : RayIntersection) = this-->o
  def closestIntersection(o : RayIntersection) = this->o
  
  val direction = unscaledDirection.normalized
    
  def parametricPoint(t : Double) = origin + direction * t
    
  override def toString = "[" + origin + "," + direction + "]"

  def -->(o : RayIntersection) = o.intersect(this)
  def ->(o : RayIntersection) = o.closestIntersection(this)

  def closestPoint(that : LineSegment) = Linear.closestPoint(
    this.origin, this.direction,
    that.start, that.end - that.start) map {
    case (u, v) => (max(u, 0.0), min(max(v, 0.0), 1.0))
  }

  def closestPoint(that : Ray) = Linear.closestPoint(
    this.origin, this.direction,
    that.origin, that.direction) map {
    case (u, v) => (max(u, 0.0), max(v, 0.0))
  }
}

object Ray {
  def fromPoints(p0 : Vector3, p1 : Vector3) = new Ray(p0, p1 - p0)
}