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

package gov.sandia.phoenix.eop

import java.io._
import java.net._
import java.util.logging.Logger

import scala.collection.immutable._
import scala.io._

/**
 * http://www.celestrak.com/SpaceData/eop19620101.txt
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
object CelestrakEOPReader {
  val logger = Logger.getLogger(getClass.getName)

  final def apply(is : InputStream) = read(Source.fromInputStream(is).getLines().toList, Map.empty[Int, EOPEntry])

//  final def apply(t : JulianDate) : EOPEntry = this(t.toModifiedJulianDate)
//  final def apply(mjd : Double) : EOPEntry = eops.getOrElse(mjd.toInt, {
//      logger.warning("No Earth Orientation Parameters for MJD: " + mjd + ". Using empty values."); EmptyEOP
//    })

  final def read(s : List[String], m : Map[Int, EOPEntry]) : Map[Int, EOPEntry] = s match {
    case Nil => m
    case "BEGIN OBSERVED" :: tail => observed(tail, m)
    case "BEGIN PREDICTED" :: tail => predicted(tail, m)
    case head :: tail => read(tail, m)
  }

  final def observed(s : List[String], m : Map[Int, EOPEntry]) : Map[Int, EOPEntry] = s match {
    case Nil => m
    case "END OBSERVED" :: tail => read(tail, m)
    case CelestrakEOPReader(eop) :: tail => observed(tail, m+(eop.mjd->eop))
    case _ => throw new Exception("Bad match. This should never happen.")
  }

  final def predicted(s : List[String], m : Map[Int, EOPEntry]) : Map[Int, EOPEntry] = s match {
    case Nil => m
    case "END PREDICTED" :: tail => read(tail, m)
    case CelestrakEOPReader(eop) :: tail => predicted(tail, m+(eop.mjd->eop))
    case _ => throw new Exception("Bad match. This should never happen.")
  }

  final def unapply(s : String) = {
    val values = s.trim.replaceAll("\\s+", ",").split(",")
    if(values.length != 13)
      None
    else
      Some(new CelestrakEarthOrientationParameters(values(0).toInt, values(1).toInt,
        values(2).toInt, values(3).toInt, values(4).toDouble,
        values(5).toDouble, values(6).toDouble, values(7).toDouble,
        values(8).toDouble, values(9).toDouble, values(10).toDouble,
        values(11).toDouble, values(12).toInt))
  }
}

object CelestrakEarthOrientationParameters extends
  EarthOrientationParameters(CelestrakEOPReader(URI.create("http://www.celestrak.com/SpaceData/eop19620101.txt").toURL.openStream))
{
  private val logger = Logger.getLogger(getClass.getName)
}

/**
 * http://www.celestrak.com/SpaceData/eop19620101.txt
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
object EmptyEOP extends CelestrakEarthOrientationParameters(0, 0, 0, 0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0)

/**
 * http://www.celestrak.com/SpaceData/eop19620101.txt
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
class CelestrakEarthOrientationParameters(val year : Int,
                                 val month : Int,
                                 val day : Int,
                                 val mjd : Int,
                                 x : Double,
                                 y : Double,
                                 dUT1 : Double,
                                 LOD : Double,
                                 val dPsi : Double,
                                 val dEpsilon : Double,
                                 val dX : Double,
                                 val dY : Double,
                                 DAT : Int) extends EOPEntry(dUT1, x, y, DAT, LOD) {
  override def toString = "EOP Values " + year + ", " + month + ", " + day + 
    ", " + mjd + ", " + ", " + x + ", " + y + ", " + dUT1 + ", " + LOD + ", " +
    dPsi + ", " + dEpsilon + ", " + dX + ", " + dY + ", " + DAT
}