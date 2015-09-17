package gov.sandia.phoenix.propagators

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.time._

/**
 * <p>A simple propagator for propagating an ECI State Vector.
 * <p>
 * @author <a href="mailto:markbastian@gmail.com">Mark Bastian</a>
 */
case class ECIStateVectorPropagator(epoch: JD,
  state: ECIStateVector) extends Propagator {
  def state(t: JD) = Some(state.state(t.delta(epoch).doubleValue()))
  def pretty = epoch + ": " + state
}