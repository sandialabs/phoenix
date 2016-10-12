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
