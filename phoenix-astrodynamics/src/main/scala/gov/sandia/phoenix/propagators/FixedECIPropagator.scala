package gov.sandia.phoenix.propagators

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.time.JD

class FixedECIPropagator(val fixedState : ECIStateVector) extends Propagator {
  def state(t : JD) = Some(fixedState)
}