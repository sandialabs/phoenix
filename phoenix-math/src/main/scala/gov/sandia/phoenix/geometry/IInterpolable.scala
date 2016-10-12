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

trait IInterpolable {
  def interpolate(x : Double) : Vector3
  def sample(pointsPerSegment : Int, inclusive : Boolean = false) = (inclusive match {
    case true => (0 to pointsPerSegment) map { i => interpolate(i.toDouble / pointsPerSegment) }
    case false => (0 until pointsPerSegment) map { i => interpolate(i.toDouble / pointsPerSegment) }
  }).toArray
}
