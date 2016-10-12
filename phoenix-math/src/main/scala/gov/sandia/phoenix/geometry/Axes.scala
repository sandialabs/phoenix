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

import scala.collection.immutable.{Vector => ImmutableArray}

object Axes {
  def XY(x : Vector3, y : Vector3) = {
    val u = x.normalized
    val w = (x ⨯ y).normalized
    val v = w ⨯ u
    this(u, v, w)
  }

  def XZ(x : Vector3, z : Vector3) = {
    val u = x.normalized
    val v = (z ⨯ x).normalized
    val w = u ⨯ v
    this(u, v, w)
  }

  def YX(y : Vector3, x : Vector3) = {
    val v = y.normalized
    val w = (x ⨯ y).normalized
    val u = v ⨯ w
    this(u, v, w)
  }

  def YZ(y : Vector3, z : Vector3) = {
    val v = y.normalized
    val u = (y ⨯ z).normalized
    val w = u ⨯ v
    this(u, v, w)
  }

  def ZX(z : Vector3, x : Vector3) = {
    val w = z.normalized
    val v = (z ⨯ x).normalized
    val u = v ⨯ w
    this(u, v, w)
  }

  def ZY(z : Vector3, y : Vector3) = {
    val w = z.normalized
    val u = (y ⨯ z).normalized
    val v = w ⨯ u
    this(u, v, w)
  }

  def apply(u: Vector3, v: Vector3, w: Vector3) =
    new RotationMatrix(ImmutableArray(
      u.x, v.x, w.x,
      u.y, v.y, w.y,
      u.z, v.z, w.z)).toQuaternion

  def getInstance = this
}
