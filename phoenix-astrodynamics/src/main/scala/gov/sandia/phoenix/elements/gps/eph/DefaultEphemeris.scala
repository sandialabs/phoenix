package gov.sandia.phoenix.elements.gps.eph

import gov.sandia.phoenix.constants._
import java.util.logging.Logger

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
object DefaultEphemeris {
  val logger = Logger.getLogger(getClass.getName)
}

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
class DefaultEphemeris(
  val PRN : Int,
  val issueOfDataClock : Int, //iodc
  val issueOfData : Byte, //(iode)
  val timeOfEphemeris : Float,          // Time of ephemeris (toe).
  val meanAnomaly : Double,        // Mean anomaly at refernce time (m0).
  val delta_n : Float,    // Mean motion difference from computed value (delta_n).
  val eccentricity : Double,         // Eccentricity (e).
  val squareRootOfSemiMajorAxis : Double,    // Square root of semi-major axis (sqrt_a).
  val longitudeOfAscendingNode : Double,   // Longitude of ascending node of orbit plane at weekly epoch. (omega_0)
  val inclination : Double,        // Inclination angle at reference time (Ix).
  val argumentOfPerigee : Double,         // Argument of perigee (w).
  val rateOfRightAscension : Double, // Rate of right ascension (omega_dot).
  val rateOfInclination : Double,     // Rate of inclination angle (i_dot).
  val cuc : Float,        // Amplitude of the cosine harmonic correction term to the argument of latitude.
  val cus : Float,        // Amplitude of the sine harmonic correction term to the argument of latitude.
  val crc : Float,        // Amplitude of the cosine harmonic correction term to the orbit radius.
  val crs : Float,        // Amplitude of the sine harmonic correction term to the orbit radius.
  val cic : Float,        // Amplitude of the cosine harmonic correction term to the angle of inclination.
  val cis : Float,        // Amplitude of the sine harmonic correction term to the angle of inclination.
  val tgd : Float,        // Estimated group delay differential.
  val timeOfCollection : Float,          // Clock data reference time. (toc)
  val af2 : Float,        // Polynomial coefficient (SV clock correction)
  val af1 : Float,        // Polynomial coefficient (SV clock correction)
  val af0 : Float,        // Polynomial coefficient (SV clock correction)
  val weekNumber : Int,   //Week number of ephemeris
  val l2code : Byte,
  val svacc : Byte,
  val l2pdata : Boolean,
  val SOH : Byte) extends AbstractEphemeris {

  def epochUTC = {
    //val depoch = (weekNumber + 1024.0) * Time.SEC_PER_WEEK + timeOfEphemeris
    //if((timeOfEphemeris < 4 * Time.SEC_PER_HR) && (timeOfCollection > (Time.SEC_PER_WEEK - 4 * Time.SEC_PER_HR))) depoch + Time.SEC_PER_WEEK else depoch
    val depoch = weekNumber * Time.SEC_PER_WEEK + timeOfEphemeris.doubleValue()
    if((timeOfEphemeris < 4 * Time.SEC_PER_HR) && (timeOfCollection > (Time.SEC_PER_WEEK - 4 * Time.SEC_PER_HR))) depoch + Time.SEC_PER_WEEK else depoch
  }

  def meanMotionDifference = delta_n
  def L2Code = 0
  def L2PDataFlag = l2pdata
  def SVAccuracy : Byte = 0
  def SVHealth = SOH
}