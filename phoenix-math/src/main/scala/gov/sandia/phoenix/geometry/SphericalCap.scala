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

case class SphericalCap(h : Double, r : Double) {
  //Plane distance from center of sphere
  def a = sqrt(h * (2.0 * r - h))
  def volume = Pi * h * (3.0 * a * a + h * h) / 6.0
  def area = 2.0 * Pi * r * h
}
