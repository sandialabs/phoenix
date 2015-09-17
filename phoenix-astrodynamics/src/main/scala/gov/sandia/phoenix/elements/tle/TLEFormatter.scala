package gov.sandia.phoenix.elements.tle

import scala.annotation.tailrec
import java.text.DecimalFormat
import gov.sandia.phoenix.time.JD

/**
 * Provides formatting capabilities to represent fields that match the TLE Format.
 *
 * @author jvtrigu
 */
object TLEFormatter {
  val epochFormat = new DecimalFormat("000.00000000")
  val floatWithDecimalFormat = new DecimalFormat(".00000000") {
    setPositivePrefix("+")
  }
  val impliedDecimalFormat = new DecimalFormat(".00000E0") {
    setPositivePrefix("+")
  }
  val noDecimalFormat = new DecimalFormat(".0000000")
  val space = ' '
  val zero  = '0'

  /**
   *
   * [[http://en.wikipedia.org/wiki/Two-line_element_set TLE Format]]
   */
  def format(L1NoradID: String, 
             classification: String, 
             launchYear: String, 
             launchNumber: Int, 
             launchPiece: String,
             epoch: JD, 
             meanMotionFirstDerivative: Double, 
             meanMotionSecondDerivative: Double, 
             bStarDrag : Double, 
             ephemerisType: Int, 
             elementNumber: Int,
             inclination: Double, 
             rightAscension: Double, 
             eccentricity: Double, 
             argumentOfPerigee: Double, 
             meanAnomaly: Double, 
             meanMotion: Double, 
             revolutionNumberAtEpoch: Int): String = {

    // Line 1
    val l1 = new StringBuilder("1 ")
    l1.append(padRight(L1NoradID,5,space))
      .append(classification).append(space)
      .append(launchYear)
      .append(padLeft(launchNumber.toString,3,zero))
      .append(padRight(launchPiece,3,space)).append(space)
      .append(formatEpoch(epoch)).append(space)
      .append(formatFloatWithDecimal(meanMotionFirstDerivative)).append(space)
      .append(formatFloatWithImpliedDecimal(meanMotionSecondDerivative)).append(space)
      .append(formatFloatWithImpliedDecimal(bStarDrag)).append(space)
      .append(ephemerisType).append(space)
      .append(padLeft(elementNumber.toString,4,space)).append(space)
      .append(0)

    // Line 2
    val l2 = new StringBuilder("2 ")
    l2.append(padRight(L1NoradID,5,space)).append(space)
      .append(formatCustomFloat(inclination,3,4)).append(space)
      .append(formatCustomFloat(rightAscension,3,4)).append(space)
      .append(formatFloatWithoutDecimal(eccentricity)).append(space)
      .append(formatCustomFloat(argumentOfPerigee,3,4)).append(space)
      .append(formatCustomFloat(meanAnomaly,3,4)).append(space)
      .append(formatCustomFloat(meanMotion,2,8))
      .append(padLeft(revolutionNumberAtEpoch.toString,5,space))
      .append(0)

    TLEUtils.fixline(l1.toString()) + "\n" + TLEUtils.fixline(l2.toString())
  }

  /**
   * Converts a floating point number into this format: +00000+0.
   * @todo add example
   * @return formatted float as string
   */
  def formatFloatWithImpliedDecimal(value: Double): String = impliedDecimalFormat.format(value).replace(".", "") match {
    case s if s.substring(s.indexOf('E')).contains('-') => s.replace("E", "")
    case s => s.replace("E", "+")
  }

  /**
   * @todo add example
   * @param value
   * @return
   */
  def formatFloatWithoutDecimal( value: Double ): String = noDecimalFormat.format(value).replace(".","")

  /**
   * @todo add example
   * @param value
   * @return
   */
  def formatFloatWithDecimal(value : Double ) : String = floatWithDecimalFormat.format(value)

  /**
   * Formats a given epoch into a format specified in the TLE requirements.
   * <pre>YYDDD.HHHHHHHH</pre>
   * Where YY are the last two digits of the year, D is the day of the year 0-36[5-6], and H is the
   * fractional part of that day.
   * @return epoch converted to TLE epoch format as string
   */
  def formatEpoch(epoch : JD ) : String = epoch.toGregorianDate.year.toString.substring(2) +
    epochFormat.format(epoch.getDayOfYear)

  /**
   * Generally specify how you want a floating point number to look, by defining the number of places to be shown at the
   * right and left side of the decimal.
   * @param value floating point value to be transformed
   * @param wholePlaces number of places to show on the left side of the decimal
   * @param fractionPlaces number of places to show on the right side of the decimal
   * @return formatted float as a string
   */
  def formatCustomFloat( value: Double, wholePlaces: Int, fractionPlaces: Int ): String = {
    val format = padLeft("",wholePlaces, zero) + "." + padRight("",fractionPlaces, zero)
    val customFormatter = new DecimalFormat(format)
    customFormatter.format(value)
  }

  /**
   * Pads the left side of a given string with <code>padWith</code>, until the total length of the of the string is
   * <code>size</code>
   * @param input string to perform padding on
   * @param size desired width of the ending string
   * @param padWith character to use for padding
   * @return padded string
   */
  def padLeft(input: String, size: Int, padWith : Char ) : String = pad(input,size,padWith,reverse = true)

  /**
   * Pads the left side of a given string with <code>padWith</code>, until the total length of the of the string is
   * <code>size</code>
   * @param input string to perform padding on
   * @param size desired width of the ending string
   * @param padWith character to use for padding
   * @return padded string
   */
  def padRight(input: String, size: Int, padWith : Char) : String = pad(input,size,padWith,reverse = false)

  /**
   * Pads a given string with <code>padWith</code>, until the total length of the of the string is <code>size</code>
   * @param input string to perform padding on
   * @param size desired width of the ending string
   * @param padWith character to use for padding
   * @param reverse whether or not to reverse string before padding
   * @return padded string
   */
  private def pad( input: String, size: Int, padWith: Char, reverse : Boolean ) : String = {
    @tailrec
    def padder( output: String, currentSize: Int ) : String = {
      if ( currentSize >= size ) output
      else padder( output + padWith, output.length + 1)
    }
    if( reverse ) padder(input.reverse, input.length).reverse
    else padder(input, input.length)
  }

}
