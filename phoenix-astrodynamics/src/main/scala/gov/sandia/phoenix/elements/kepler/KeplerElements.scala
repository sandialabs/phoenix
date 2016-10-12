/*
 * Copyright (c) 2016 Sandia Corporation. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by the
 * terms of this license.
 * You must not remove this notice, or any other, from this software.
 *
 * Contributors:
 * - Mark Bastian: Original author.
 * - See Git logs.
 */

package gov.sandia.phoenix.elements.kepler

import gov.sandia.phoenix.constants._
import gov.sandia.phoenix.elements.ElementFunctions
import gov.sandia.phoenix.elements.kepler.KeplerElements._
import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.elements.tle.TLEFormatter
import gov.sandia.phoenix.geometry._
import gov.sandia.phoenix.math._
import gov.sandia.phoenix.numerics.Numerics._
import gov.sandia.phoenix.propagators.KeplerElementsPropagator
import gov.sandia.phoenix.time._

import scala.math._

/**
 * [[http://www.lns.cornell.edu/~seb/celestia/orbital-parameters.html Element Description]]
 * [[http://www.amsat.org/amsat/keps/kepmodel.html AMSAT Kepler Elements Tutorial]]
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
trait KeplerElements extends ElementFunctions {
  def p : Double
  def e : Double
  def inclination : Angle

  def eccentricAnomaly : ANOMALY
  lazy val meanAnomaly = eccentricAnomaly match {
    case ECCENTRIC_ANOMALY(_E) => ECCENTRIC_ANOMALY(_E - e * sin(_E))
    case PARABOLIC_ANOMALY(_B) => PARABOLIC_ANOMALY(_B + _B * _B * _B / 3.0)
    case HYPERBOLIC_ANOMALY(_H) => HYPERBOLIC_ANOMALY(e * sinh(_H) - _H)
  }
  def nu(dt : Double) = anomalyToNu(e, anomaly(dt))
//  def state : ECIStateVector
//  def state(dt : Double) : ECIStateVector
  def orientation : Quaternion

  def incrads = inclination.radians

  val orbitType = if(e == 0) CIRCULAR else if(e < 1) ELLIPTICAL else if(e == 1) PARABOLIC else HYPERBOLIC
  val a = orbitType match {
    case CIRCULAR => p
    case ELLIPTICAL => p / (1 - e * e)
    case PARABOLIC => Double.PositiveInfinity
    case HYPERBOLIC => p / (1 - e * e)
  }

  lazy val suborbital = r_perigee <= WGS84.R_EQ_M

//  lazy val isAtApogee = !(abs(Pi - nu) > 1.0E-6)
//  lazy val isAtPerigee = !(abs(nu) > 1.0E-6)
  
  override val period = 2.0 * Pi * sqrt(a * a * a / WGS84.GM)

  val n = orbitType match {
    case PARABOLIC => 2.0 * sqrt(WGS84.GM / (p * p * p))
    case _ => sqrt(WGS84.GM / abs(a * a * a))
  }
  
  def anomaly(dt : Double) = meanAnomaly match {
    case ECCENTRIC_ANOMALY(_M0) => KepEqtnE(_M0 + n * dt, e)
    case PARABOLIC_ANOMALY(_B) => KepEqtnP(dt, p)
    case HYPERBOLIC_ANOMALY(_M0) => KepEqtnH(_M0 + n * dt, e)
  }

  lazy val r_apogee = orbitType match {
    case PARABOLIC => a
    case _ => a * (1.0 + e)
  }

  lazy val r_perigee = orbitType match {
    case PARABOLIC => 0.5 * p
    case _ => a * (1.0 - e)
  }

  def apogee : ECIStateVector
  def perigee : ECIStateVector
  def ascendingNode : ECIStateVector
  def descendingNode : ECIStateVector

//  def isAtAscendingNode : Boolean
//  def isAtDescendingNode : Boolean
  
  def OMEGA : Angle
  def omega : Angle
  def flightPathAngle : Angle
  def nu : Angle

  def toPropagator(epoch : JD) = new KeplerElementsPropagator(this, epoch)

  def changeInclinationAndOmega(i : Angle, O : Angle) = KeplerElements.changeInclinationAndOmega(this, i, O)

  def pretty = {
    "type: " + orbitType.toString.replace("()", "") +
    "\nsemi-parameter: " + p + " m" +
    "\naxis: " + a + " m" +
    "\nperiod: " + period + " s" +
    "\neccentricity: " + e +
    "\ninclination: " + inclination.degrees + "\u00B0" +
    "\neccentric anomaly: " + eccentricAnomaly.value.toDegrees + "\u00B0" +
    "\nmean anomaly: " + meanAnomaly.value.toDegrees + "\u00B0"
  }

  /**
   * Generates a TLE formatted string using kepler elements
   */
  def toTLEFormat(noradID: String, epoch: JD, bstar : Double = 0.0 ) : String = {
    // Missing defaults
    val classification = "U"
    val launchYear = "00"
    val launchNumber = 0
    val launchPiece = "0"
    val meanMotionPrime = 0.0
    val meanMotionDoublePrime = 0.0
    val elementSetType = 0
    val elementNumber = 0

    val orbitInclination = inclination.degrees
    val rightAscensionOfAscendingNode = OMEGA.degrees
    val eccentricity = e
    val argumentOfPerigee = omega.degrees
    val meanAnomaly = this.meanAnomaly.value.toDegrees
    val meanMotion = Time.SEC_PER_DAY * n / (2.0 * scala.math.Pi)
    val revNumber = 0

    TLEFormatter.format(
      noradID,
      classification,
      launchYear,
      launchNumber,
      launchPiece,
      epoch,
      meanMotionPrime,
      meanMotionDoublePrime,
      bstar,
      elementSetType,
      elementNumber,
      orbitInclination,
      rightAscensionOfAscendingNode,
      eccentricity,
      argumentOfPerigee,
      meanAnomaly,
      meanMotion,
      revNumber
    )
  }
}

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
object KeplerElements {
  /**
   * See Vallado, p. 125
   */
//  def RandV(p : Double, e : Double, i : Double, OMEGA : Double, omega : Double, 
//            nu : Double, u : Double, lambda_true : Double, omega_true : Double) : ECIStateVector =
//              if(e == 0.0 && i == 0.0)
//                RandV(p, e, i, 0.0, 0.0, lambda_true)
//  else if(e == 0.0)
//    RandV(p, e, i, OMEGA, 0.0, u)
//  else if(i == 0.0)
//    RandV(p, e, i, 0.0, omega_true, nu)
//  else RandV(p, e, i, OMEGA, omega, nu)

  /**
   * See Vallado, p. 125
   */
  def RandV(p : Double, e : Double, i : Angle,
            OMEGA : Angle, omega : Angle, nu : Angle) = {
    val sinNu = nu.sin
    val cosNu = nu.cos
    val den = 1 / (1 + e * cosNu)
    val pre = sqrt(WGS84.GM / p)
    val q = OMEGA.rz * i.rx * omega.rz
    val r = q * Vector3(p * cosNu * den, p * sinNu * den, 0.0)
    val v = q * Vector3(-pre * sinNu, pre * (e + cosNu), 0.0)
    ECIStateVector(r, v)
  }

  def q(OMEGA : Angle, inclination : Angle, omega : Angle) = OMEGA.rz * inclination.rx * omega.rz
  
  /**
   * See Vallado, p. 85
   */
  def nuToAnomaly(e : Double, nu : Angle) : ANOMALY = if(e < 1.0) {
    val E = atan2(nu.sin * sqrt(1 - e * e), e + nu.cos)
    ECCENTRIC_ANOMALY(if(E < 0) E + 2 * Pi else E)
  } else if(e > 1)
    HYPERBOLIC_ANOMALY(atanh(nu.sin * sqrt(e * e - 1) / (e + nu.cos)))
  else
    PARABOLIC_ANOMALY(nu.half.tan)

  /**
   * See Vallado, p. 85
   */
  def anomalyToNu(e : Double, anomaly : ANOMALY, p : Double = 0.0, r : Double = 0.0) : Angle = anomaly match {
    case ECCENTRIC_ANOMALY(_E) => {
        val nu = Angle.atan2(sin(_E) * sqrt(1 - e * e), cos(_E) - e)
        if(nu.radians < 0) nu + Angle.TwoPi else nu
      }
    case PARABOLIC_ANOMALY(_B) => Angle.atan2(_B * p, p - r)
    case HYPERBOLIC_ANOMALY(_H) => Angle.atan2(-sinh(_H) * sqrt(e * e - 1), cosh(_H) - e)
  }

  private final def Estep(M : Double, e : Double)(E : Double) = E + (M - E + e * sin(E)) / (1 - e * cos(E))
  private final def Hstep(M : Double, e : Double)(H : Double) = H + (M - e * sinh(H) + H) / (e * cosh(H) - 1)

  /**
   * Algorithm 2, p. 73 of Vallado
   */
  def KepEqtnE(M : Double, e : Double) = ECCENTRIC_ANOMALY(iterate(Estep(M, e)) {
      if((M < 0 && -Pi < M) || M > Pi) M - e else M + e
    })

  /**
   * Algorithm 3, p. 77 of Vallado
   */
  def KepEqtnP(dt : Double, p : Double) = PARABOLIC_ANOMALY({
      val np = 2.0 * sqrt(WGS84.GM / (p * p * p))
      val s = acot(1.5 * np * dt) * 0.5
      val w = atan(cbrt(tan(s)))
      2.0 * cot(2.0 * w)
    })

  /**
   * Algorithm 4, p. 79 of Vallado
   */
  def KepEqtnH(M : Double, e : Double) = HYPERBOLIC_ANOMALY(iterate(Hstep(M, e)) {
      if(e < 1.6) {
        if((M > -Pi && M < 0) || M > Pi) M - e else M + e
      } else if(e < 3.6 && abs(M) > Pi) M - signum(M) * e else M / (e - 1)
    })

  /**
   * Equations 2-93 and 2-94 of Vallado
   */
  def flightPathAngle(e : Double, nu : Angle) = Angle.atan2(e * nu.sin, 1 + e * nu.cos)

  /**
   * See Algorithm 11 of Vallado
   */
  def timeOfFlight(ri : Vector3, rf : Vector3, p : Double) = {
    val ni = ri.normalized
    val nf = rf.normalized
    val cosdNu = ni * nf
    val sindNu = (ni % nf).mag
    val dNu = atan2(sindNu, cosdNu)
    val rim = ri.mag
    val rfm = rf.mag
    val rr = rim * rfm
    val k = rr * (1.0 - cosdNu)
    val l = rim + rfm
    val m = rr * (1 + cosdNu)
    val a = m * k * p / ((2.0 * m - l * l) * p * p + 2.0 * k * l * p - k * k)
    val f = 1.0 - rfm / p * (1 - cosdNu)
    val g = rr * sindNu / sqrt(WGS84.GM * p)

    if(a > 0.0) {
      val df = sqrt(WGS84.GM / p) * tan(0.5 * dNu) * ((1.0 - cosdNu) / p - 1.0 / rim - 1.0 / rfm)
      val cosdE = 1.0 - rim / a * (1.0 - f)
      val sindE = - rr * df / sqrt(WGS84.GM * a)
      g + sqrt(a * a * a / WGS84.GM) * (atan2(sindE, cosdE) - sindE)
    } else if(a == 0.0) {
      val c = sqrt(rim * rim + rfm * rfm - 2.0 * rr * cosdNu)
      val s = 0.5 * (l + c)
      2.0 / 3.0 * sqrt(0.5 * s * s * s / WGS84.GM) * (1.0 - pow((s - c) / s, 1.5))
    } else {
      val coshdH = 1.0 + (f - 1.0) * rim / a
      val dH = acosh(coshdH)
      g + sqrt(-a * a * a / WGS84.GM) * (sinh(dH) - dH)
    }
  }

  def changeInclinationAndOmega(elements : KeplerElements, i : Angle, O : Angle) = if(elements.e > 1E-6) None else Some {
    val i_initial = elements.inclination
    val OMEGA_initial = elements.OMEGA
    val i_final = i
    val OMEGA_final = O
    val tOMEGA = OMEGA_final - OMEGA_initial
    val dOMEGA = if(tOMEGA.radians < -Pi) tOMEGA + Angle.TwoPi else tOMEGA

    val burnAngle = Angle.acos(i_initial.cos * i_final.cos + i_initial.sin * i_final.sin * dOMEGA.cos)
    val cu_initial = (i_final.sin * dOMEGA.cos - burnAngle.cos * i_initial.sin) / (burnAngle.sin * i_initial.cos)
    val u_initial = if(dOMEGA.radians > 0) Angle.acos(cu_initial) else Angle.Pi - Angle.acos(cu_initial)
    //val u_initial = signum(dOMEGA)*acos(cu_initial)

    val cu_final = (i_initial.cos * i_final.sin - i_initial.sin * i_final.cos * dOMEGA.cos) / burnAngle.sin
//    val u_final = signum(dOMEGA)*acos(cu_final)
    val u_final = if(dOMEGA.radians > 0) Angle.acos(cu_final) else Angle.Pi - Angle.acos(cu_final)
    (new CircularInclinedKeplerElements(elements.p, i_initial, OMEGA_initial, u_initial).state,
     new CircularInclinedKeplerElements(elements.p, i_final, OMEGA_final, u_final).state)
  }

  def createGEO(lon : Double, lat : Double, epoch : JD) = {
    val eciPos = epoch.GEOtoECI(Geodetic(lon, lat, Orbit.GEO_RADIUS_M - WGS84.R_EQ_M))
    val perp = Z_AXIS % eciPos
    val axis = if (perp.mag < 1.0E-6) Y_AXIS else perp.normalized
    val v = axis * Orbit.GEO_VELOCITY_M_PER_SEC
    ECIStateVector(eciPos, v).keplers.toPropagator(epoch)
  }
}

abstract sealed class ANOMALY(val value : Double)
case class ECCENTRIC_ANOMALY(E : Double) extends ANOMALY(E)
case class PARABOLIC_ANOMALY(B : Double) extends ANOMALY(B)
case class HYPERBOLIC_ANOMALY(H : Double) extends ANOMALY(H)

abstract sealed class ORBIT_TYPE
case object CIRCULAR extends ORBIT_TYPE
case object ELLIPTICAL extends ORBIT_TYPE
case object PARABOLIC extends ORBIT_TYPE
case object HYPERBOLIC extends ORBIT_TYPE