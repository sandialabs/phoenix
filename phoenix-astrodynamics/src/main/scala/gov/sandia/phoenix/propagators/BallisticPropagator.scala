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