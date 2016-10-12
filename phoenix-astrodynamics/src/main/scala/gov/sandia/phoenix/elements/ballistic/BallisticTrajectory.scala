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

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.propagators.{ECIStateVectorPropagator, GeodeticPropagator}
import gov.sandia.phoenix.time._

/**
 * Description of a ballistic trajectory over a given interval.
 */
case class BallisticTrajectory(interval: Interval, launchState: ECIStateVector) {
  lazy val srcGeo = launchState.toECEF(interval.start).position.toGeodetic
  lazy val dstGeo = interval.end.ECItoECEF(ECIStateVectorPropagator(interval.start, launchState).unsafe_position(interval.end)).toGeodetic
  lazy val launchDeltaV = (launchState.velocity - GeodeticPropagator(srcGeo).unsafe_velocity(interval.start)).mag
  def pretty = "Launch from " + srcGeo + " to " + dstGeo + " over " + interval + " (" + (interval.getDurationSeconds / 60) + " min)"
}