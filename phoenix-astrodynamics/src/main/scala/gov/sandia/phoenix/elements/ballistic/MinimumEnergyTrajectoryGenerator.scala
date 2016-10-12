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

package gov.sandia.phoenix.elements.ballistic

import gov.sandia.phoenix.geometry.Geodetic
import gov.sandia.phoenix.numerics.optimization._
import gov.sandia.phoenix.orbits.FINAL_STATE
import gov.sandia.phoenix.propagators.GeodeticPropagator
import gov.sandia.phoenix.time.JD

object MinimumEnergyTrajectoryGenerator {
  def fromStartTime(src: Geodetic, tgt: Geodetic, epoch: JD) = {
    val startState = GeodeticPropagator(src).unsafe_state(epoch)
    val launchSolution = { dt: Double => startState.intercept(FINAL_STATE(GeodeticPropagator(tgt).unsafe_state(epoch plusSeconds dt)), dt, true) }
    val (dt, dv) = goldenMin(2.0 * 60.0, 3.0 * 60.0 * 60.0, 1.0)(launchSolution(_).deltaV)
    val solution = launchSolution(dt)
    val t_final = epoch plusSeconds dt
    BallisticTrajectory(epoch until t_final, solution.initialTransferState)
  }

  def fromEndTime(src: Geodetic, tgt: Geodetic, epoch: JD) = {
    val endState = GeodeticPropagator(src).unsafe_state(epoch)
    val launchSolution = { dt: Double => GeodeticPropagator(src).unsafe_state(epoch minusSeconds dt).intercept(FINAL_STATE(endState), dt, true) }
    val (dt, dv) = goldenMin(2.0 * 60.0, 3.0 * 60.0 * 60.0, 1.0)(launchSolution(_).deltaV)
    val solution = launchSolution(dt)
    val t_initial = epoch minusSeconds dt
    BallisticTrajectory(t_initial until epoch, solution.initialTransferState)
  }
}