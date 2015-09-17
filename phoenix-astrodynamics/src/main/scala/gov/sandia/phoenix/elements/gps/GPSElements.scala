package gov.sandia.phoenix.elements.gps

import gov.sandia.phoenix.time._

object GPSElements {
  val MU = 3.986005E14
  val D_OMEGA_E = 7.2921151467E-5
}

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
trait GPSElements {
  def argumentOfPerigee : Double
  def cic : Float
  def cis : Float
  def crc : Float
  def crs : Float
  def cuc : Float
  def cus : Float
  def eccentricity : Double
  def epochUTC : Double
  def epochJD = epoch.doubleValue
  def inclination : Double
  def longitudeOfAscendingNode : Double
  def meanAnomaly : Double
  def meanMotionDifference : Double
  def rateOfInclination : Double
  def rateOfRightAscension : Double
  def semiMajorAxis : Double
  def PRN : Int
  def SOH : Byte
  def epoch = GPS_EPOCH plusSeconds epochUTC
}

/**
 * Orbital Elements as defined for Ephemeris, Almanac, or Keplerian 
 * elements.  At this point, the definitive guide as to what is an element is 
 * found in the ICD200 spec.
 * 
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 *
 */
trait OrbitalElements
{
  def epochUTC : Double
  def epochJD : Double
}
