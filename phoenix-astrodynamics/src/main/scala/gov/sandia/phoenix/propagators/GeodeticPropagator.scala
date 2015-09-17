package gov.sandia.phoenix.propagators

import gov.sandia.phoenix.elements.sv.ECEFStateVector
import gov.sandia.phoenix.geometry.{Geodetic, Vector3}
import gov.sandia.phoenix.time.JD

case class GeodeticPropagator(location : Geodetic) extends Propagator {
  val stationaryECEFState = new ECEFStateVector(location.toECEF, Vector3(0, 0, 0))
  def state(t : JD) = Some(stationaryECEFState.toECI(t))
}
