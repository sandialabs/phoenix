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

package gov.sandia.phoenix.eclipses

import gov.sandia.phoenix.geometry.{InfiniteCone, LineSegment, Sphere, Vector3}

import scala.math._

case class Umbra(source : Sphere, occluder : Sphere) {
  val apex = LineSegment(source.center, occluder.center).extrapolate(1.0 + occluder.radius / (source.radius - occluder.radius))
  val direction = (source.center - occluder.center).normalized
  val angle = asin((source.radius - occluder.radius) / (source.center dist occluder.center))
  val cone = InfiniteCone(apex, direction, angle)
  val clip = occluder.tangentPlane(apex)

  def contains(v : Vector3) = cone.contains(v) && clip.forall(p => p <= v)
}