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