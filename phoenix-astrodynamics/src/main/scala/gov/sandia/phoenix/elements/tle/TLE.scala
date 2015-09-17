package gov.sandia.phoenix.elements.tle

import gov.sandia.phoenix.elements.sgp4.MeanElements
import gov.sandia.phoenix.propagators.sgp4.{WGS72, GravityConstants}
import gov.sandia.phoenix.time._

/**
 * The data format used to represent orbital elements that describe a body (typically a satellite)
 * orbiting around the Earth.
 *
 * @param line0 Optional line describing the TLE
 */
case class TLE(line0 : Option[String], line1 : String, line2 : String, gc : GravityConstants = WGS72) extends MeanElements {
  //Line1 accessors
  def noradNumber1 = SATELLITE_FIELD(line1).value.getOrElse("")
  def classification = CLASSIFICATION_FIELD(line1).value.getOrElse("")
  def launchYear = INTERNATIONAL_DESIGNATOR_YEAR_FIELD(line1).value.getOrElse("")
  def launchNumber = INTERNATIONAL_DESIGNATOR_LAUNCH_NUMBER_FIELD(line1).value.getOrElse("")
  def launchPiece = INTERNATIONAL_DESIGNATOR_LAUNCH_PIECE_FIELD(line1).value.getOrElse("")
  def epochyr = EPOCH_YEAR_FIELD(line1).value.getOrElse(0)
  def epochdays = EPOCH_DAY_FIELD(line1).value.getOrElse(0.0)
  def bstar = BSTAR_DRAG_FIELD(line1).value.getOrElse(0.0)

  //Line2 accessors
  def noradNumber2 = SATELLITE_FIELD(line2).value.getOrElse("")
  def inclination = INCLINATION_FIELD(line2).value.getOrElse(0.0).toRadians
  def rightAscension = RIGHT_ASCENSION_FIELD(line2).value.getOrElse(0.0).toRadians
  def eccentricity = ECCENTRICITY_FIELD(line2).value.getOrElse(0.0)
  def argumentOfPerigee = ARGUMENT_OF_PERIGEE_FIELD(line2).value.getOrElse(0.0).toRadians
  def meanAnomaly = MEAN_ANOMALY_FIELD(line2).value.getOrElse(0.0).toRadians
  def meanMotion = MEAN_MOTION_FIELD(line2).value.getOrElse(0.0) / TLEUtils.MINUTES_PER_REVOLUTION //Units are radians / minute
  def revnum = REVOLUTION_NUMBER_FIELD(line2).value.getOrElse(0)

  def noradID = noradNumber1

  def internationalDesignator = launchYear + "-" + launchNumber + launchPiece

  def epoch = TimeBuilder(epochyr, 1, 0).plusDays(epochdays)

  def name = line0 match {
    case Some(l0) => l0
    case None => noradID
  }

  def valid = (TLEUtils.checksumError(line1) == 0) && (TLEUtils.checksumError(line2) == 0)
  def fixchecksum = if (valid) this else TLE(line0, TLEUtils.fixline(line1), TLEUtils.fixline(line2))

  def format = line0 match {
    case Some(x) => x + "\n" + line1 + "\n" + line2
    case None => line1 + "\n" + line2
  }

  def I0 = inclination
  def Ω0 = rightAscension
  def e0 = eccentricity
  def ω0 = argumentOfPerigee
  def bStarDrag = bstar
}