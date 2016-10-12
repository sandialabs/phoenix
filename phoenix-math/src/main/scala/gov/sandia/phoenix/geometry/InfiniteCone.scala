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
 * A representation of a cone that extends forever.
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class InfiniteCone(point : Vector3, dir : Vector3, theta : Double) {
  val direction = dir.normalized
  def contains(v : Vector3) = angularSeparation(v) <= theta
  def angularSeparation(v : Vector3) = acos(cosAngularSeparation(v))
  def cosAngularSeparation(v : Vector3) = min(max((v - point).normalized * direction, -1.0), 1.0)
  
  def intersect(sphere : Sphere) = for {
    d <- direction.sweep(theta, 100)
    intersection = new Ray(point, d)->sphere
    if intersection != null
      } yield intersection
  }
