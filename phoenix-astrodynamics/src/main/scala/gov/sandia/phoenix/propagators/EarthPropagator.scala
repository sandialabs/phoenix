package gov.sandia.phoenix.propagators

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry.ORIGIN
import gov.sandia.phoenix.time.JD

object EarthPropagator extends Propagator {
  override def state(t: JD): Option[ECIStateVector] = Some(ECIStateVector(ORIGIN, ORIGIN))
  def getInstance = this
}
