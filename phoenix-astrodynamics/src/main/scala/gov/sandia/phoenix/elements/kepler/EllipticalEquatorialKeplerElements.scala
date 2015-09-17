package gov.sandia.phoenix.elements.kepler

import gov.sandia.phoenix.elements.kepler.KeplerElements._
import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry._

case class EllipticalEquatorialKeplerElements(p : Double,
                                         e : Double, 
                                         nu : Angle,
                                         omega_true : Angle) extends KeplerElements {
  val inclination = Angle.ZERO
  val eccentricAnomaly = nuToAnomaly(e, nu)
  val OMEGA = Angle.NaN
  val omega = Angle.NaN
  val flightPathAngle = KeplerElements.flightPathAngle(e, nu)

  override lazy val apogee = new EllipticalEquatorialKeplerElements(p, e, Angle.Pi, omega_true).state
  override lazy val perigee = new EllipticalEquatorialKeplerElements(p, e, Angle.ZERO, omega_true).state
  override lazy val ascendingNode = new EllipticalEquatorialKeplerElements(p, e, omega_true.explement, omega_true).state
  override lazy val descendingNode = new EllipticalEquatorialKeplerElements(p, e,omega_true.supplement, omega_true).state
  lazy val orientation = q(Angle.ZERO, inclination, omega_true)
  
  override def state : ECIStateVector = RandV(p, e, inclination, Angle.ZERO, omega_true, nu)
  override def state(dt : Double) : ECIStateVector = new EllipticalEquatorialKeplerElements(p, e, nu(dt), omega_true).state

//  override lazy val isAtAscendingNode = true
//  override lazy val isAtDescendingNode = true

  override def pretty = super.pretty +
  "\ntrue longitude of periapsis: " + omega_true +
  "\ntrue anomaly: " + nu
}
