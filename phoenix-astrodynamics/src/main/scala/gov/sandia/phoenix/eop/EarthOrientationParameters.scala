package gov.sandia.phoenix.eop

import java.util.logging.Logger

import gov.sandia.phoenix.eop.EarthOrientationParameters._
import gov.sandia.phoenix.numerics._
import gov.sandia.phoenix.time._

import scala.collection.immutable._

object EarthOrientationParameters {
  val logger = Logger.getLogger(getClass.getName)
}

class EarthOrientationParameters(val eops : Map[Int, EOPEntry]) {
  val min = eops.keySet.min
  val max = eops.keySet.max
  
  final def apply(t : JD) : EOPEntry = this(t.toMJD)

  final def apply(mjd : Double) : EOPEntry = {
    val mjda = mjd.toInt
    val mjdb = mjda + 1
    val eopa = this(mjda)
    val eopb = this(mjdb)
    val x = Interpolation.linterp(mjda, eopa.x, mjdb, eopb.x, mjd)
    val y = Interpolation.linterp(mjda, eopa.y, mjdb, eopb.y, mjd)
    val dut1 = Interpolation.linterp(mjda, eopa.dUT1, mjdb, eopb.dUT1, mjd)
    val LOD = Interpolation.linterp(mjda, eopa.LOD, mjdb, eopb.LOD, mjd)
    new EOPEntry(dut1, x, y, eopa.DAT, LOD)
  }

  final def apply(mjd : Int) = eops.getOrElse(mjd, {
    if(mjd < min || mjd > max) logger.warning("No Earth Orientation Parameters for MJD: " + mjd + ". Using empty values.")
    EmptyEOPEntry
  })
}

class EOPEntry(val dUT1 : Double, val x : Double, val y : Double, 
               val DAT : Int, val LOD : Double = 0.0) {
  override def toString = "EOP dUT1 = " + dUT1 + ", x = " + x + ", y = " + y + ", DAT = " + DAT + ", LOD = " + LOD
}

object EmptyEOPEntry extends EOPEntry(0.0, 0.0, 0.0, TAI.leapseconds(TAI.leapseconds.lastKey).toInt)