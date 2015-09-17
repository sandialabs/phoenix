package gov.sandia.phoenix.propagators

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.solarsystem.Luna
import gov.sandia.phoenix.time.JD

object MoonPropagator extends Propagator {
  override def state(t: JD): Option[ECIStateVector] = {
    val position = Luna.position(t)
    val delta = 60
    val position2 = Luna.position(t plusSeconds delta)
    val velocity = (position2 - position) / delta
    Some(ECIStateVector(position, velocity))
  }
  def getInstance = this
}

