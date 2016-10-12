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

package gov.sandia.phoenix.elements.tle

import scala.util.matching.Regex
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import java.text.DecimalFormat

object TleField {
  def osubstring(line : String, start : Int, end : Int) = if(line.length < end) None else Some(line.substring(start, end))

  def partially_extract[T](line : String, field : TleField[T])(f : String => TleExtractionResult[T]) =
    field.extract_raw(line) match {
      case Some(raw_field) => f(raw_field)
      case None => TleExtractionResult(None, "", line, field, TLE_EXTRACTION_ERROR("Line is not long enough"))
    }

  def extract_any[T](line : String, field : TleField[T])(f : String => T) = partially_extract(line, field){
    case field.regex(raw_field, _*) => Try(f(raw_field)) match {
      case Success(i) => TleExtractionResult(Some(f(raw_field)), raw_field, line, field, TLE_EXTRACTION_SUCCESS)
      case Failure(_) => TleExtractionResult(None, raw_field, line, field,
        TLE_EXTRACTION_ERROR("Non-parseable field does not conform to expected pattern of " + field.plainPattern + "."))
    }
    case raw_field => Try(f(raw_field)) match {
      case Success(i) => TleExtractionResult(Some(i), raw_field, line, field,
        TLE_EXTRACTION_WARNING("Format does not conform to expected pattern of " + field.plainPattern + "."))
      case Failure(_) => TleExtractionResult(None, raw_field, line, field,
        TLE_EXTRACTION_ERROR("Non-parseable field does not conform to expected pattern of " + field.plainPattern + "."))
    }
  }
}

sealed abstract class TleField[T](val name : String, val startIndex : Int, val endIndex : Int, val regex : Regex, val plainPattern : String) {
  def extract_raw(line : String) = TleField.osubstring(line, startIndex, endIndex)
  def proc(s : String) : T
  def apply(line : String) = TleField.extract_any(line, this){ proc }
  def length = endIndex - startIndex
  val isSpace = false
}

trait TleDoubleField { def proc(s : String) = s.trim.toDouble }
trait TleIntField { def proc(s : String) = s.trim.toInt }
trait TleStringField { def proc(s : String) = s.trim }

//Extractors for either line
class SPACE_FIELD(pos : Int) extends TleField[String]("Space at Position " +
  new DecimalFormat("00").format(pos), pos, pos + 1, """(\s)""".r, " ") with TleStringField {
  override val isSpace = true
}
object LINE_NUMBER_FIELD extends TleField[Int]("Line Number", 0, 1, """(\d)""".r, "N") with TleIntField
object SPACE_01_FIELD extends SPACE_FIELD(1)
object SATELLITE_FIELD extends TleField[String]("Satellite Number", 2, 7, """(\d{5})""".r, "NNNNN") with TleStringField
object CHECKSUM_FIELD extends TleField[Int]("Checksum", 68, 69, """(\d?)""".r, "N") with TleIntField {
  override def apply(line : String) = {
    val preliminaryResult = super.apply(line)
      preliminaryResult.value match {
        case Some(reportedChecksum) => TLEUtils.checksum(line.substring(0, TLEConstants.LINE_LENGTH)) match {
        case computedChecksum if computedChecksum == reportedChecksum => preliminaryResult
        case computedChecksum => TleExtractionResult(preliminaryResult.value, preliminaryResult.raw_string,
          preliminaryResult.line, preliminaryResult.field, TLE_EXTRACTION_WARNING("Checksum digit should be " + computedChecksum))
      }
      case None => preliminaryResult
    }
  }
}

//Line 1 only
object LINE_1_FIELD extends TleField[Int]("Line 1", 0, 1, """(1)""".r, "1") with TleIntField
object CLASSIFICATION_FIELD extends TleField[String]("Classification", 7, 8, """(.{1})""".r, "C") with TleStringField
object SPACE_08_FIELD extends SPACE_FIELD(8)
object INTERNATIONAL_DESIGNATOR_YEAR_FIELD extends TleField[String]("International Designator Year", 9, 11, """([\d\s]{2})""".r, "NN") with TleStringField
object INTERNATIONAL_DESIGNATOR_LAUNCH_NUMBER_FIELD extends TleField[String]("International Designator Launch Number", 11, 14, """([\d\s]{3})""".r, "NNN") with TleStringField
object INTERNATIONAL_DESIGNATOR_LAUNCH_PIECE_FIELD extends TleField[String]("International Designator Launch Piece", 14, 17, """([\w\s]{3})""".r, "AAA") with TleStringField
object SPACE_17_FIELD extends SPACE_FIELD(17)
object EPOCH_YEAR_FIELD extends TleField[Int]("Epoch Year", 18, 20, """(\d{2})""".r, "NN"){
  def proc(raw_string : String) = TLEUtils.fix2DigitYear(raw_string.trim.toInt)
}
object EPOCH_DAY_FIELD extends TleField[Double]("Epoch Day", 20, 32, """(\d{3}\.\d{8})""".r, "NNN.NNNNNNNN") with TleDoubleField
object SPACE_32_FIELD extends SPACE_FIELD(32)
object MEAN_MOTION_FIRST_DERIVATIVE_FIELD extends TleField[Double]("First Derivative of Mean Motion", 33, 43, """([\s+-0]\.[\d]{8})""".r, "+.NNNNNNNN") with TleDoubleField
object SPACE_43_FIELD extends SPACE_FIELD(43)
object MEAN_MOTION_SECOND_DERIVATIVE_FIELD extends TleField[Double]("Second Derivative of Mean Motion", 44, 52, """(([\s+-])([\d\.\s+-]{5}+)([+-]\d))""".r, "+NNNNN-N"){
  def proc(raw_string : String) = {
    val regex(_, sign, significand, exponent) = raw_string
    (sign + "0." + significand + "E" + exponent).trim.toDouble
  }
}
object SPACE_52_FIELD extends SPACE_FIELD(52)
object BSTAR_DRAG_FIELD extends TleField[Double]("BStar Drag", 53, 61, """(([\s+-])([\d\.\s+-]{5}+)([+-]\d))""".r, "+NNNNN-N"){
  def proc(raw_string : String) = {
    val regex(_, sign, significand, exponent) = raw_string
    (sign + "0." + significand + "E" + exponent).trim.toDouble
  }
}
object SPACE_61_FIELD extends SPACE_FIELD(61)
object EPHEMERIS_TYPE_FIELD extends TleField[String]("Ephemeris Type", 62, 63, """([\d\s]{1})""".r, "N") with TleStringField
object SPACE_63_FIELD extends SPACE_FIELD(63)
object ELEMENT_NUMBER_FIELD extends TleField[Int]("Element Number", 64, 68, """([\d\s]{4})""".r, "NNNN") with TleIntField

//Line 2
object LINE_2_FIELD extends TleField[Int]("Line 2", 0, 1, """(2)""".r, "2") with TleIntField
object SPACE_07_FIELD extends SPACE_FIELD(7)
object INCLINATION_FIELD extends TleField[Double]("Inclination", 8, 16, """([\d\.\s+-]{8})""".r, "NNN.NNNN") with TleDoubleField
object SPACE_16_FIELD extends SPACE_FIELD(16)
object RIGHT_ASCENSION_FIELD extends TleField[Double]("Right Ascension", 17, 25, """([\d\.\s+-]{8})""".r, "NNN.NNNN") with TleDoubleField
object SPACE_25_FIELD extends SPACE_FIELD(25)
object ECCENTRICITY_FIELD extends TleField[Double]("Eccentricity", 26, 33, """([\d\s+-]{7})""".r, "NNNNNNN"){ raw_string =>
  def proc(raw_string : String) = ("0." + raw_string).trim.toDouble
}
object SPACE_33_FIELD extends SPACE_FIELD(33)
object ARGUMENT_OF_PERIGEE_FIELD extends TleField[Double]("Argument of Perigee", 34, 42, """([\d\.\s+-]{8})""".r, "NNN.NNNN") with TleDoubleField
object SPACE_42_FIELD extends SPACE_FIELD(42)
object MEAN_ANOMALY_FIELD extends TleField[Double]("Mean Anomaly", 43, 51, """([\d\.\s+-]{8})""".r, "NNN.NNNN") with TleDoubleField
object SPACE_51_FIELD extends SPACE_FIELD(51)
object MEAN_MOTION_FIELD extends TleField[Double]("Mean Motion", 52, 63, """([\s\d]{2}+\.\d{8})""".r, "NN.NNNNNNN") with TleDoubleField
object REVOLUTION_NUMBER_FIELD extends TleField[Int]("Revolution Number at Epoch", 63, 68, """([\s\d]{5})""".r, "NNNNN") with TleIntField