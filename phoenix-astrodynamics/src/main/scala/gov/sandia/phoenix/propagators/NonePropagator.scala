package gov.sandia.phoenix.propagators

import gov.sandia.phoenix.time.JD

object NonePropagator extends Propagator {
  override def state(t : JD) = None
  def getInstance = this
}
