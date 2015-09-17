package gov.sandia.phoenix.misc

import java.io.File

import gov.sandia.phoenix.access.Access
import gov.sandia.phoenix.elements.tle.TLEUtils
import gov.sandia.phoenix.geometry.{Degrees, Geodetic, Vector3}
import gov.sandia.phoenix.linalg.Matrix
import gov.sandia.phoenix.propagators.sgp4.SGP4
import gov.sandia.phoenix.time.{JD, TimeBuilder}

import scala.collection._
import scala.math._

abstract sealed class DOPQuality
case class IDEAL_DOP(dop : Double) extends DOPQuality
case class EXCELLENT_DOP(dop : Double) extends DOPQuality
case class GOOD_DOP(dop : Double) extends DOPQuality
case class MODERATE_DOP(dop : Double) extends DOPQuality
case class FAIR_DOP(dop : Double) extends DOPQuality
case class POOR_DOP(dop : Double) extends DOPQuality

/**
 * See: [[http://en.wikipedia.org/wiki/Dilution_of_precision_(GPS) Wikipedia Entry]]
 * See: [[http://www.nrem.iastate.edu/class/assets/nrem446_546/week3/Dilution_of_Precision.pdf External Article]]
 *
 * Note that if HDOP and VDOP are wanted, you must convert to an SEZ frame.
 */
class DilutionOfPrecision(val t : JD, val rpos : Geodetic, val spos : Iterable[Vector3]) {
  val sez = rpos.toSEZFrame
  val directions = (spos map { p => sez.toSEZ(p).normalized } map { d => Array(d.x, d.y, d.z, -1.0) }).toArray
  val A = if(directions.length > 0) Some(new Matrix(directions.toArray)) else None
  val Q = A flatMap { a => a.transpose * a flatMap { _.invert } }
  val PDOP = Q map { q => sqrt(q(0, 0) + q(1, 1) + q(2, 2)) }
  val TDOP = Q map { q => sqrt(q(3, 3)) }
  val GDOP = TDOP flatMap { tdop => PDOP map { pdop => sqrt(pdop * pdop + tdop * tdop) } }
  val HDOP = Q map { q => sqrt(q(0, 0) + q(1, 1)) }
  val ZDOP = Q map { q => sqrt(q(2, 2)) }

  override def toString = "PDOP: " + PDOP + " TDOP: " + TDOP + " GDOP: " + GDOP + " HDOP: " + HDOP + " ZDOP: " + ZDOP
}

object DilutionOfPrecision extends App {
  val t = TimeBuilder(2013, 3, 14, 15)
  val c = TLEUtils.extract(new File("mytles.tle")) map { tle => SGP4(tle) } take 4
  val p = Geodetic(0, 0, 0)
  //  val numCombos = 8
  //
  val r = t.GEOtoECI(p)
  val s = c flatMap { prop => prop.position(t) } filter { x => Access(p, t.ECItoECEF(x), Degrees(5.0)) }
  val ecefs = s.map(t.ECItoECEF)
  val dop = new DilutionOfPrecision(t, p, ecefs.take(9))
  println(dop)

  //  val sezDops = ecefs.toList.combinations(numCombos) map { new DilutionOfPrecision(t, p, _) }
  //  val sezSorted = sezDops.toList sortBy { dop => dop.PDOP }
  //  println(sezSorted.head)
}
