package gov.sandia.phoenix.solarsystem

import gov.sandia.phoenix.constants._
import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry.{Sphere, _}
import gov.sandia.phoenix.sp.{TwoBody, SPForceProvider}
import gov.sandia.phoenix.time._

import scala.math._

object Sol extends SPForceProvider {
  /**
   * Compute the geocentric position vector of the Sun.  In other words, the 
   * vector from of this point would point from the center of the Earth to the
   * Sun.  This is in an inertial frame.  Units are AU.  To go to meters, 
   * scale by an AU.
   * @param t
   * @return
   */
  def position(t : JD) =
  {
//    val tut = t.toJulianCenturyJ2000
//    val lambda = IEEEremainder(280.460 + 36000.770 * tut, 360.0)
//    val M = IEEEremainder(357.5277233 + 35999.05034 * tut, 360.0)
//    val lambdaEcliptic = toRadians(lambda + 1.914666471 * sin(toRadians(M)) + 0.019994643 * sin(2.0 * toRadians(M)))
//    val r = 1.000140612 - 0.016708617 * cos(toRadians(M)) - 0.000139589 * cos(2.0 * toRadians(M))
//    val epsilon = toRadians(IEEEremainder(23.439291 - 0.0130042 * tut, 360.0))
    val (lambda, m, lambdaEcliptic, epsilon) = t.solarParams
    val r = 1.000140612 - 0.016708617 * cos(m) - 0.000139589 * cos(2.0 * m)
    Vector3(r * cos(lambdaEcliptic), r * cos(epsilon) * sin(lambdaEcliptic),
            r * sin(epsilon) * sin(lambdaEcliptic))
  }
  
  def posECEF(t : JD) = {
    val eci = (!t.fk5.precession * position(t)) * Celestial.AU
    t.fk5.J2000toITRF(eci)
  }

  def state(t : JD) = {
    val deltaT = 60 * 10
    val pos = eciPosition(t)
    val pos2 = eciPosition(t plusSeconds deltaT)
    val vel = (pos2 - pos) / deltaT
    Some(ECIStateVector(pos, vel))
  }
  
  /**
   * Compute the normalized direction vector from the Earth to the Sun.
   */
  def direction(t : JD) = eciPosition(t).normalized

  def eciPosition(t : JD) = (!t.fk5.precession * position(t)) * Celestial.AU

  def sphere(t : JD) = Sphere(eciPosition(t), Celestial.SOLAR_RADIUS)

  def lambda(t : Double) = ((280.4606184 + 36000.77005361 * t) % 360.0).toRadians
  def M(t : Double) = ((357.5277233 + 35999.05034 * t) % 360.0).toRadians
  def lambdaEcliptic(λ : Double, M : Double) = (λ.toDegrees + 1.914666471 * sin(M) + 0.019994643 * sin(2.0 * M)).toRadians
  def epsilon(t : Double) = ((23.439291 - 0.0130042 * t) % 360.0).toRadians

  /**
   * www.dtic.mil/cgi-bin/GetTRDoc?AD=AD0785380
   * http://www.amostech.com/TechnicalPapers/2011/NROC/HEJDUK.pdf
   */
  def visualMagnitude(observer : Vector3, observed : Vector3, sunPos : Vector3, d: Double = 1.0, rho: Double = 0.4) = {
    val observer2observed = observer - observed
    val cosPhi = observer2observed.normalized * (sunPos - observed).normalized
    val phi = acos(cosPhi)

    val A = Pi * d * d / 4.0
    val num = 2.0 * A * rho * ((Pi - phi) * cosPhi + sin(phi))
    val den = 3.0 * Pi * Pi

    val R = observer2observed.mag
    val Ms = -26.74
    Ms - 2.5 * log10(num / den) + 5.0 * log10(R)
  }

  def limitingVisualDiameter(observer : Vector3, observed : Vector3, sunPos : Vector3, Mv : Double = 1.0, rho: Double = 0.4) = {
    val observer2observed = observer - observed
    val cosPhi = observer2observed.normalized * (sunPos - observed).normalized
    val phi = acos(cosPhi)

    val num = 2.0 * rho * ((Pi - phi) * cosPhi + sin(phi))
    val den = 3.0 * Pi * Pi

    val R = observer2observed.mag
    val Ms = -26.74
    val limitingCrossSectionArea = den * pow(10, (Ms + 5.0 * log10(R) - Mv) / 2.5) / num
    sqrt(4.0 * limitingCrossSectionArea / Pi)
  }

  val mu = 132712440018.0
  val GM = mu * 1E9
  def acceleration(t : JD, state : ECIStateVector) =
    TwoBody.acceleration(state.position, position(t) * Celestial.AU, GM)
}

sealed trait TWILIGHT_TYPE { def zeta : Double }
case object Twilight extends TWILIGHT_TYPE { val zeta = 90.0 + 50.0 / 60.0 }
case object Civil extends TWILIGHT_TYPE { val zeta = 96.0 }
case object Nautical extends TWILIGHT_TYPE { val zeta = 102.0 }
case object Astronomical extends TWILIGHT_TYPE { val zeta = 108.0 }

sealed trait RISE_SET_TYPE {
  def offset : Double
  def adjustLHA(LHA_sunset : Double) : Double
}
case object Sunrise extends RISE_SET_TYPE {
  val offset = 0.25
  def adjustLHA(LHA_sunset : Double) = 360.0 - LHA_sunset
}
case object Sunset extends RISE_SET_TYPE {
  val offset = 0.75
  def adjustLHA(LHA_sunset : Double) = LHA_sunset
}
