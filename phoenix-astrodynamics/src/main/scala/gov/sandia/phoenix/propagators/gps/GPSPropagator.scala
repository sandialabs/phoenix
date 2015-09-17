package gov.sandia.phoenix.propagators.gps

import gov.sandia.phoenix.elements.sv.ECEFStateVector
import gov.sandia.phoenix.time.{GPS, GPS_EPOCH, JD}
import gov.sandia.phoenix.constants.Time
import scala.math._
import gov.sandia.phoenix.geometry.Vector3
import gov.sandia.phoenix.propagators.Propagator
import gov.sandia.phoenix.elements.gps.GPSElements

/**
 * Propagator for the position and velocity of a GPS satellite.  Based in part on code
 * from http://www.ngs.noaa.gov/gps-toolbox/bc_velo.htm and the icd spec.
 */
case class GPSPropagator(elements : GPSElements) extends Propagator {
  def state(t : JD) = Some(getPosVelECEF(t).toECI(t))

  //Although a big iteration cap is provided, it is rare to iterate more
  //than 3 or 4 times.
  def solveKep(Mk : Double, iterations : Int, maxError : Double) : Array[Double] = {
    val f = solveKep(Mk, maxError)_
    f(scala.Double.MaxValue, Mk, sin(Mk), cos(Mk), iterations)
  }

  def solveKep(Mk : Double, maxError : Double)(error : Double, Ek : Double, sinEk : Double, cosEk : Double, iterations : Int) : Array[Double] =
    if(abs(error) <= maxError || iterations == 0) Array(sinEk, cosEk) else
    {
      val newError = (Mk - Ek + elements.eccentricity * sinEk) / (elements.eccentricity * cosEk - 1.0)
      val newEk = Ek - newError
      solveKep(Mk, maxError)(newError, newEk, sin(newEk), cos(newEk), iterations-1)
    }

  def getPosVelECEF(t : JD) = {
    //Time, in seconds, since GPS epoch
    val tDelta = (t.doubleValue - GPS_EPOCH.doubleValue) * Time.SEC_PER_DAY
    val t_k = tDelta - elements.epochUTC + GPS(t)

    val n_0 = sqrt(GPSElements.MU / (elements.semiMajorAxis * elements.semiMajorAxis * elements.semiMajorAxis))
    val n = n_0 + elements.meanMotionDifference
    val Mk = elements.meanAnomaly + t_k * n

    //Solve for Ek using Newton's method
    val res = solveKep(Mk, 20, 1E-10)
    val sinEk = res(0)
    val cosEk = res(1)

    val omecEk = 1.0 - elements.eccentricity * cosEk

    val num = sqrt(1.0 - elements.eccentricity * elements.eccentricity) * sinEk
    val den = cosEk - elements.eccentricity

    val ekdot = n / omecEk
    val nu = atan2(num, den)
    val sinNu = sin(nu)
    val cosNu = cos(nu)
    val nudot = sinEk * ekdot * (1.0 + elements.eccentricity * cosNu) / (sinNu * omecEk)

    val PHI_k = nu + elements.argumentOfPerigee
    val sin2Phi_k = sin(2.0 * PHI_k)
    val cos2Phi_k = cos(2.0 * PHI_k)

    val corr_u = elements.cus * sin2Phi_k + elements.cuc * cos2Phi_k
    val corr_r = elements.crs * sin2Phi_k + elements.crc * cos2Phi_k
    val corr_i = elements.cis * sin2Phi_k + elements.cic * cos2Phi_k

    val u_k = PHI_k + corr_u
    val sinU_k = sin(u_k)
    val cosU_k = cos(u_k)
    val sin2U_k = sin(2.0 * u_k)
    val cos2U_k = cos(2.0 * u_k)

    val i_k = elements.inclination + corr_i + elements.rateOfInclination * t_k
    val sinI_k = sin(i_k)
    val cosI_k = cos(i_k)

    val OMEGA_kdot = elements.rateOfRightAscension - GPSElements.D_OMEGA_E
    val OMEGA_k = elements.longitudeOfAscendingNode + OMEGA_kdot * t_k - GPSElements.D_OMEGA_E * (elements.epochUTC % Time.SEC_PER_WEEK)
    val sinOMEGA_k = sin(OMEGA_k)
    val cosOMEGA_k = cos(OMEGA_k)

    val ukdot = nudot + 2.0 * (elements.cus * cos2U_k - elements.cuc * sin2U_k) * nudot
    val rkdot = elements.semiMajorAxis * elements.eccentricity * sinEk * ekdot + 2.0 * (elements.crs * cos2U_k - elements.crc * sin2U_k) * nudot
    val ikdot = elements.rateOfInclination + (elements.cis * cos2U_k - elements.cic * sin2U_k) * 2.0 * nudot

    val r_k = elements.semiMajorAxis * omecEk + corr_r

    val xpk = r_k * cosU_k
    val ypk = r_k * sinU_k

    val x = xpk * cosOMEGA_k - ypk * cosI_k * sinOMEGA_k
    val y = xpk * sinOMEGA_k + ypk * cosI_k * cosOMEGA_k
    val z = ypk * sinI_k

    val xpkdot = rkdot * cosU_k - ypk * ukdot
    val ypkdot = rkdot * sinU_k + xpk * ukdot
    val t0 = xpkdot - ypk * cosI_k * OMEGA_kdot
    val t1 = xpk * OMEGA_kdot + ypkdot * cosI_k - ypk * sinI_k * ikdot
    val xkdot = t0 * cosOMEGA_k - t1 * sinOMEGA_k
    val ykdot = t0 * sinOMEGA_k + t1 * cosOMEGA_k
    val zkdot = ypkdot * sinI_k + ypk * cosI_k * ikdot

    new ECEFStateVector(Vector3(x, y, z), Vector3(xkdot, ykdot, zkdot))
  }
}
