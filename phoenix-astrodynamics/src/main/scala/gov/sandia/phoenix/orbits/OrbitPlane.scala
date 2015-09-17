package gov.sandia.phoenix.orbits

import gov.sandia.phoenix.elements.kepler.KeplerElements
import gov.sandia.phoenix.geometry.{Degrees, _}
import gov.sandia.phoenix.solarsystem.Sol
import gov.sandia.phoenix.time.JD

trait OrbitPlaneFunctions extends IPlane {
  def inclination : Angle
  def rightAscension : Angle

  def direct = inclination.degrees >= 0.0 && inclination.degrees < 90.0
  def prograde = direct
  def retrograde = !direct

  def perigeePoint(omega : Angle, range : Double) = KeplerElements.q(rightAscension, inclination, omega) * (X_AXIS * range)

  def apogeePoint(omega : Angle, range : Double) = perigeePoint(omega + Degrees(180.0), range)

  def phase(angle : Angle) = new OrbitPlane((rightAscension + angle).constrainUnsigned, inclination)

  def beta(t : JD) = Angle.acos(Sol.direction(t) * n).complement
}

case class OrbitPlane(rightAscension : Angle, inclination : Angle) extends OrbitPlaneFunctions {
  val d = 0.0
  val n = rightAscension.rz * inclination.rx * Z_AXIS
}
