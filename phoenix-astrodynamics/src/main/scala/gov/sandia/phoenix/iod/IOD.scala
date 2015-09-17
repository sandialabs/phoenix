package gov.sandia.phoenix.iod

import gov.sandia.phoenix.constants._
import gov.sandia.phoenix.elements.sv.{AzElRStateVector, ECIStateVector}
import gov.sandia.phoenix.geometry._
import gov.sandia.phoenix.time._

import scala.math._

/**
 * Some of the initial orbit determination functions from Vallado.
 */
object IOD {
  def RAZEL(pv : ECIStateVector, site : Geodetic, t : JD) = {
    val siteFrame = site.toSEZFrame
    val ecef = pv.toECEF(t)

    val pSEZ = siteFrame.toSEZ(ecef.position)
    val pAzElR = siteFrame.toAzElR(ecef.position)
    val vSEZ = !siteFrame.rotation * ecef.velocity

    val dR = pSEZ * vSEZ / pSEZ.mag
    val den = pSEZ.x * pSEZ.x + pSEZ.y * pSEZ.y
    val dAz = (vSEZ.x * pSEZ.y - vSEZ.y * pSEZ.x) / den
    val dEl = (vSEZ.z - dR * sin(toRadians(pAzElR.elevation))) / sqrt(den)

    new AzElRStateVector(pAzElR, AzElR(toDegrees(dAz), toDegrees(dEl), dR))
  }

  def Gibbs(r1 : Vector3, r2 : Vector3, r3 : Vector3) = {
    val Z12 = r1 ⨯ r2
    val Z23 = r2 ⨯ r3
    val Z31 = r3 ⨯ r1

    val m1 = r1.mag
    val m2 = r2.mag
    val m3 = r3.mag

    val coplanarity = toDegrees(Pi * 0.5 - acos(Z23*r1/(Z23.mag * m1)))
    if(abs(coplanarity) > 3) None else {
      val N = Z23 * m1 + Z31 * m2 + Z12 * m3
      val D = Z12 + Z23 + Z31
      val S = r1 * (m2 - m3) + r2 * (m3 - m1) + r3 * (m1 - m2)
      val B = D ⨯ r2
      val L = sqrt(WGS84.mu_M / (D.mag * N.mag))
      Some((B / m2 + S) * L)
    }
  }

  def p(r1 : Vector3, r2 : Vector3, r3 : Vector3) = {
    val Z12 = r1 ⨯ r2
    val Z23 = r2 ⨯ r3
    val Z31 = r3 ⨯ r1

    val m1 = r1.mag
    val m2 = r2.mag
    val m3 = r3.mag

    val coplanarity = toDegrees(Pi * 0.5 - acos(Z23*r1/(Z23.mag * m1)))
    if(abs(coplanarity) > 3) None else Some {
      val N = Z23 * m1 + Z31 * m2 + Z12 * m3
      val D = Z12 + Z23 + Z31
      val S = r1 * (m2 - m3) + r2 * (m3 - m1) + r3 * (m1 - m2)
      (N.mag / D.mag, S.mag / D.mag)
    }
  }

  def HGibbs(r1 : Vector3, r2 : Vector3, r3 : Vector3, t1 : JD, t2 : JD, t3 : JD) = {
    val dt31 = (t3.doubleValue - t1.doubleValue) * Time.SEC_PER_DAY
    val dt32 = (t3.doubleValue - t2.doubleValue) * Time.SEC_PER_DAY
    val dt21 = (t2.doubleValue - t1.doubleValue) * Time.SEC_PER_DAY

    val Z23 = r2 ⨯ r3

    val m1 = r1.mag
    val m2 = r2.mag
    val m3 = r3.mag

    val coplanarity = toDegrees(Pi * 0.5 - acos(Z23*r1/(Z23.mag * m1)))
    if(abs(coplanarity) > 3) None else {
      Some(r1 * (-dt32 * (1 / (dt21 * dt31) + WGS84.mu_M / (12 * m1 * m1 * m1))) +
        r2 * ((dt32 - dt21) * (1 / (dt21 * dt32) + WGS84.mu_M / (12 * m2 * m2 * m2))) +
        r3 * (dt21 * (1 / (dt32 * dt31) + WGS84.mu_M / (12 * m3 * m3 * m3))))
    }
  }
}
