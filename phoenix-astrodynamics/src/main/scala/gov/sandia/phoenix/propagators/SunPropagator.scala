package gov.sandia.phoenix.propagators

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.solarsystem.Sol
import gov.sandia.phoenix.time.JD

object SunPropagator extends Propagator {
  override def state(t: JD): Option[ECIStateVector] = Sol.state(t)
  def getInstance = this
}
