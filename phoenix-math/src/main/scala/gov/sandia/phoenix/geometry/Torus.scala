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

case class Torus(R : Double, r : Double) {
  def dist(p : Vector3) = {
    val a = R - hypot(p.x, p.y)
    sqrt(a * a + p.z * p.z) - r
  }

  def contains(p : Vector3) = dist(p) <= 0

  def area = 4.0 * Pi * Pi * R * r
  def volume = 2.0 * Pi * Pi * R * r * r
}