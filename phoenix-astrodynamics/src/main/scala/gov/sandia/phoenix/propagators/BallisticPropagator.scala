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

package gov.sandia.phoenix.propagators

import gov.sandia.phoenix.elements.ballistic.BallisticTrajectory
import gov.sandia.phoenix.time.JD

case class BallisticPropagator(trajectory: BallisticTrajectory) extends Propagator {
  val preFlightPropagator = GeodeticPropagator(trajectory.srcGeo)
  val flightPropagator = ECIStateVectorPropagator(trajectory.interval.start, trajectory.launchState)
  val postFlightPropagator = GeodeticPropagator(trajectory.dstGeo)

  def state(t: JD) = t match {
    case before if t < trajectory.interval.start => preFlightPropagator.state(t)
    case after if t > trajectory.interval.end => postFlightPropagator.state(t)
    case _ => flightPropagator.state(t)
  }
}