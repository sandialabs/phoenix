package gov.sandia.phoenix.cdt

import gov.sandia.phoenix.geometry.Angle
import gov.sandia.phoenix.orbits.OrbitPlane

object Planar {
  def planar(range : Double, initialPlane : OrbitPlane, t : Int, u0 : Angle) =
    Walker.delta(range, initialPlane, t, 1, 1, u0)
}
