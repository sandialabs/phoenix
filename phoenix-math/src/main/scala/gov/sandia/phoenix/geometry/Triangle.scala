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

case class Triangle(p0 : Vector3, p1 : Vector3, p2 : Vector3) {
  val s0 = p2 - p0
  val s1 = p1 - p0
  val normal = (s0 ⨯ s1).normalized
  def plane = new Plane(p0, normal)

  def intersect(ray : Ray) = {
    val p = plane.intersect(ray)
    val u = p * s0
    val v = p * s1
    if(u >= 0 && u <= 1 && v >= 0 &&  u + v <= 1) Some(p) else None
  }
}
