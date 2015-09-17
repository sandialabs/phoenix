package gov.sandia.phoenix.propagators.sgp4

import gov.sandia.phoenix.elements.sgp4.MeanElements
import gov.sandia.phoenix.elements.sv.{ECIStateVector, TEMEStateVector}
import gov.sandia.phoenix.geometry._
import gov.sandia.phoenix.propagators.Propagator
import gov.sandia.phoenix.time.JD

import scala.math._

case class SGP4(elements: MeanElements, alwaysReturn : Boolean = true) extends Propagator {

  def state(t: JD): Option[ECIStateVector] = {
    val tsince = (t.doubleValue - elements.jdepoch) * 1440
    val v = new Array[Double](6)
    val res = sgp4(tsince, v)

    if (res == "" || alwaysReturn) {
      val eci = Vector3(v(0) * 1000, v(1) * 1000, v(2) * 1000)
      val vel = Vector3(v(3) * 1000, v(4) * 1000, v(5) * 1000)
      Some(new TEMEStateVector(eci, vel).toECI(t))
    }
    else {
      logger.severe(res)
      None
    }
  }

  def getRAAN = elements.Ω0

  def getSemimajorAxis = elements.ao * elements.gc.radiusearthkm * 1000

  def sgp4(Δt: Double, pv: Array[Double]) = {
    val vkmpersec = elements.gc.radiusearthkm * elements.gc.xke / 60.0
    elements.error = ""
    val oparams = if (elements.method == DeepSpace)
      OrbitalParameters(elements, Δt).deepSpaceConvert(elements.calcPeriodics(Δt), Δt)
    else
      OrbitalParameters(elements, Δt)

    //Complain about bad eccentricites
    if (elements.method == DeepSpace) {
      if ((oparams.eccentricity < 0.0) || (oparams.eccentricity > 1.0))
        elements.error = "Eccentricity is < 0 or > 1 (" + oparams.eccentricity + ")"
    }

    val aycof = if (elements.method == DeepSpace)
      -0.5 * elements.gc.j3oj2 * oparams.sinip
    else
      -0.5 * elements.gc.j3oj2 * sin(elements.I0)

    val xlcof = if (elements.method == DeepSpace) {
      if (abs(oparams.cosip + 1.0) > 1.5e-12)
        -0.25 * elements.gc.j3oj2 * oparams.sinip *
          (3.0 + 5.0 * oparams.cosip) / (1.0 + oparams.cosip)
      else
        -0.25 * elements.gc.j3oj2 * oparams.sinip *
          (3.0 + 5.0 * oparams.cosip) / 1.5e-12
    } else {
      // sgp4fix for divide by zero with xinco = 180 deg
      if (abs(cos(elements.I0) + 1.0) > 1.5e-12)
        -0.25 * elements.gc.j3oj2 * sin(elements.I0) *
          (3.0 + 5.0 * cos(elements.I0)) / (1.0 + cos(elements.I0))
      else
        -0.25 * elements.gc.j3oj2 * sin(elements.I0) *
          (3.0 + 5.0 * cos(elements.I0)) / (1.0 + cos(Pi - 1.0e-9))
    }

    val axnl = oparams.eccentricity * cos(oparams.argumentOfPerigee)
    val aynl = oparams.eccentricity * sin(oparams.argumentOfPerigee) + aycof / (oparams.am * (1.0 - oparams.eccentricity * oparams.eccentricity))
    val xl = oparams.meanAnomaly + oparams.argumentOfPerigee + oparams.rightAscension + xlcof * axnl / (oparams.am * (1.0 - oparams.eccentricity * oparams.eccentricity))
    val u = SGP4Util.mod2pi(xl - oparams.rightAscension)
    var eo1 = u
    var tem5 = 9999.9
    var ktr: Int = 1
    var sineo1 = 0.0
    var coseo1 = 0.0
    while ((abs(tem5) >= 1.0e-12) && (ktr <= 10)) {
      sineo1 = sin(eo1)
      coseo1 = cos(eo1)
      tem5 = 1.0 - coseo1 * axnl - sineo1 * aynl
      tem5 = (u - aynl * coseo1 + axnl * sineo1 - eo1) / tem5
      if (abs(tem5) >= 0.95) tem5 = if (tem5 > 0.0) 0.95 else -0.95
      eo1 = eo1 + tem5
      ktr = ktr + 1
    }
    val ecose = axnl * coseo1 + aynl * sineo1
    val esine = axnl * sineo1 - aynl * coseo1
    val el2 = axnl * axnl + aynl * aynl
    val pl = oparams.am * (1.0 - el2)
    var mrt = 0.0
    if (pl < 0.0)
      elements.error = "semi-latus rectum < 0.0 (" + pl + ")"
    else {
      val rl = oparams.am * (1.0 - ecose)
      val rdotl = sqrt(oparams.am) * esine / rl
      val rvdotl = sqrt(pl) / rl
      val betal = sqrt(1.0 - el2)
      val sinu = oparams.am / rl * (sineo1 - aynl - axnl * esine / (1.0 + betal))
      val cosu = oparams.am / rl * (coseo1 - axnl + aynl * esine / (1.0 + betal))
      val su = atan2(sinu, cosu)
      val sin2u = (cosu + cosu) * sinu
      val cos2u = 1.0 - 2.0 * sinu * sinu
      val temp1 = 0.5 * elements.gc.j2 / pl
      val temp2 = temp1 / pl
      val (con41, x1mth2, x7thm1) = if (elements.method == DeepSpace) {
        val cosisq = oparams.cosip * oparams.cosip
        (3.0 * cosisq - 1.0, 1.0 - cosisq, 7.0 * cosisq - 1.0)
      } else (3.0 * elements.θ * elements.θ - 1.0, 1.0 - elements.θ * elements.θ, 7.0 * elements.θ2 - 1.0)
      mrt = rl * (1.0 - 1.5 * temp2 * betal * con41) + 0.5 * temp1 * x1mth2 * cos2u
      val xnode = oparams.rightAscension + 1.5 * temp2 * oparams.cosip * sin2u
      val xinc = oparams.inclination + 1.5 * temp2 * oparams.cosip * oparams.sinip * cos2u
      val mvt = rdotl - oparams.meanMotion * temp1 * x1mth2 * sin2u / elements.gc.xke
      val rvdot = rvdotl + oparams.meanMotion * temp1 * (x1mth2 * cos2u + 1.5 * con41) / elements.gc.xke
      val sinsu = sin(su - 0.25 * temp2 * x7thm1 * sin2u)
      val cossu = cos(su - 0.25 * temp2 * x7thm1 * sin2u)
      val snod = sin(xnode)
      val cnod = cos(xnode)
      val sini = sin(xinc)
      val cosi = cos(xinc)
      val xmx = -snod * cosi
      val xmy = cnod * cosi
      val ux = xmx * sinsu + cnod * cossu
      val uy = xmy * sinsu + snod * cossu
      val uz = sini * sinsu
      pv(0) = (mrt * ux) * elements.gc.radiusearthkm
      pv(1) = (mrt * uy) * elements.gc.radiusearthkm
      pv(2) = (mrt * uz) * elements.gc.radiusearthkm
      pv(3) = (mvt * ux + rvdot * (xmx * cossu - cnod * sinsu)) * vkmpersec
      pv(4) = (mvt * uy + rvdot * (xmy * cossu - snod * sinsu)) * vkmpersec
      pv(5) = (mvt * uz + rvdot * (sini * cossu)) * vkmpersec
    }

    if (mrt < 1.0) elements.error = "Satellite orbit is in a decay condition (" + mrt + ")"

    elements.error
  }
}