package gov.sandia.phoenix.elements.kepler

import gov.sandia.phoenix.elements.kepler.KeplerElements._
import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry._

case class EllipticalInclinedKeplerElements(p : Double,
                                       e : Double, 
                                       inclination : Angle,
                                       OMEGA : Angle,
                                       omega : Angle,
                                       nu : Angle) extends KeplerElements {
  val eccentricAnomaly = nuToAnomaly(e, nu)
  val flightPathAngle = KeplerElements.flightPathAngle(e, nu)

  override lazy val apogee = new EllipticalInclinedKeplerElements(p, e, inclination, OMEGA, omega, Angle.Pi).state
  override lazy val perigee = new EllipticalInclinedKeplerElements(p, e, inclination, OMEGA, omega, Angle.ZERO).state
  override lazy val ascendingNode = new EllipticalInclinedKeplerElements(p, e, inclination, OMEGA, omega, omega.explement).state
  override lazy val descendingNode = new EllipticalInclinedKeplerElements(p, e, inclination, OMEGA, omega, omega.supplement).state
//  override lazy val isAtAscendingNode = abs(nu + omega) < 1.0E-6
//  override lazy val isAtDescendingNode = abs(Pi - nu + omega) < 1.0E-6
  lazy val orientation = q(OMEGA, inclination, omega)
  
  override def state : ECIStateVector = RandV(p, e, inclination, OMEGA, omega, nu)
  override def state(dt : Double) : ECIStateVector = new EllipticalInclinedKeplerElements(p, e, inclination, OMEGA, omega, nu(dt)).state

  def newOMEGA(newOMEGA : Angle) = new EllipticalInclinedKeplerElements(p, e, inclination, newOMEGA % Angle.TwoPi, omega, nu)
  
  override def pretty = super.pretty +
  "\nright ascension: " + OMEGA.degrees + "\u00B0" +
  "\nargument of perigee: " + omega.degrees + "\u00B0" +
  "\ntrue anomaly: " + nu.degrees + "\u00B0"
}