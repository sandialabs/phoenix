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

package gov.sandia.phoenix.sp

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry.{Axes, Vector3, Y_AXIS}
import gov.sandia.phoenix.time.{Interval, JD}

import scala.math._

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
class DeltaVForce(val interval : Interval, 
                  val ΔvFunction : (JD, ECIStateVector) => Vector3,
                  val F : Double,
                  val ṁ : Double, 
                  val m0 : Double) extends SPForceProvider {
  def acceleration(t : JD, state : ECIStateVector) = if(interval contains t) {
    ΔvFunction(t, state) * (ṁ / m(t) / (-log(1.0 - ṁ * Δt / m0)))
  } else Vector3(0, 0, 0)
  
  val Δt = interval.getDurationSeconds.doubleValue
  def m(t : JD) = 1.0
}

/**
 * A Δv in the direction the range vector crossed with the up vector. This will
 * be in track for circular orbits and in the direction of the velocity vector
 * for other orbits.
 * 
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
class AlongTrackDeltaV(interval : Interval, F : Double,  ṁ : Double, m0 : Double) 
  extends DeltaVForce(interval, (t, state) => { Axes.XY(state.position, state.velocity) * Y_AXIS }, F, ṁ, m0)
  
/**
 * A Δv in the direction of the current velocity vector.
 * 
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
class InTrackDeltaV(interval : Interval, F : Double, ṁ : Double, m0 : Double) 
  extends DeltaVForce(interval, (t, state) => { state.velocity.normalized }, F, ṁ, m0)