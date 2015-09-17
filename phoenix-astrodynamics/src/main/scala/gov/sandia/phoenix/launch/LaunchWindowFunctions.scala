package gov.sandia.phoenix.launch

import gov.sandia.phoenix.orbits.OrbitPlane

import scala.math._
import gov.sandia.phoenix.math._
import gov.sandia.phoenix.time.JD
import gov.sandia.phoenix.geometry._

object LaunchWindowFunctions {
  def launchAzimuths(inc : Angle, lat : Angle) : List[Angle] = oasin(inc.cos / lat.cos) match {
    case Some(β) => β.constrainUnsigned :: β.supplement.constrainUnsigned :: Nil
    case _ => Nil
  }

  def geometries(plane : OrbitPlane, loc : Geodetic) = launchAzimuths(plane.inclination, Degrees(loc.latitude)) map { β =>
      val λinitial = Radians(acos(β.cos / plane.inclination.sin))
      val λ = if(plane.direct == (loc.latitude >= 0.0)) λinitial else λinitial.explement
      LaunchGeometry(loc, plane, β, λ)
    }
  
  def windows(plane : OrbitPlane, loc : Geodetic, t : JD) = geometries(plane, loc) map { _.window(t) }
}
