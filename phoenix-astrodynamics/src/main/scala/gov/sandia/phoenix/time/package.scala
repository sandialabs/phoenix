package gov.sandia.phoenix

import scala.math.BigDecimal
import java.util.{TimeZone, Calendar, Date}
import gov.sandia.phoenix.constants.Time

package object time {
  def JDtoGregorianDate(julianDate : BigDecimal) = {
    val x = julianDate + 0.5
    val Z = x.toDouble.toInt
    val F = x - Z
    val A = if(Z < 2299161) Z else {
      val alpha = ((Z-1867216.25)/36524.25).toInt
      Z + 1 + alpha - alpha / 4
    }
    val B = A + 1524
    val C = ((B - 122.1) / 365.25).toInt
    val D = (365.25 * C).toInt
    val E = ((B - D) / 30.6001).toInt
    val dd = F + (B - D - (30.6001 * E).toInt)
    val mm = if(E < 13.5) E - 1 else E - 13
    val yyyy = if(mm > 2.5) C - 4716 else C - 4715
    val day = dd.toDouble.toInt
    val hh = (dd - day) * 24
    val hour = hh.toDouble.toInt
    val mmm = (hh - hour) * 60
    val minute = mmm.toDouble.toInt
    val ss = (mmm - minute) * 60
    val second = ss.toDouble.toInt
    val millis = (ss - second) * 1000

    new GregorianDate(yyyy, mm, day, hour, minute, second, millis.doubleValue)
  }

  /**
   * Compute the Julian Date equivalent of this date using the algorithm
   * described on page 60 of Astronomical Algorithms by Meeus.
   */
  def gregorianDateToJD(year : Int, month : Int, day : Int, hour : Int, minute : Int, seconds : Int, milliseconds : BigDecimal) : BigDecimal = {
    val millis = if(milliseconds.mc.getPrecision == 0)
      milliseconds(java.math.MathContext.DECIMAL128) else milliseconds
    val y = if(month <= 2) year - 1 else year
    val m = if(month <= 2) month + 12 else month
    val A = y / 100
    //Choose between Julian and Gregorian Calendars
    val B = if(year > 1582) 2 - A + A / 4 else {
      if(year < 1582) 0 else {
        if(month > 10) 2 - A + A / 4 else {
          if(month < 10) 0 else {
            if(day < 15) 0 else 2 - A + A / 4
          }
        }
      }
    }
    val D = (((millis / 1000.0 + seconds) / 60.0 + minute) / 60.0 + hour) / 24.0 + day
    D + ((365.25 * (y + 4716) + (30.6001 * (m + 1)).toInt).toInt + B - 1524.5)
  }

  def dateToJulianDate(instant : Date) : BigDecimal = {
    val cal = Calendar.getInstance(TimeZone.getTimeZone("UTC"))
    cal.setTime(instant)
    gregorianDateToJD(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH)+1,
      cal.get(Calendar.DAY_OF_MONTH), cal.get(Calendar.HOUR_OF_DAY),
      cal.get(Calendar.MINUTE), cal.get(Calendar.SECOND), BigDecimal(cal.get(Calendar.MILLISECOND), java.math.MathContext.DECIMAL128))
  }

  def JDtoJulianCentury(julianDate : BigDecimal, epoch : BigDecimal) = (julianDate - epoch) / 36525
  def JDtoJulianCenturyJ2000(julianDate : BigDecimal) = JDtoJulianCentury(julianDate, JD2000.value)

  def JDtoGMST(julianDate : BigDecimal) = {
    //    println("WARNING WARNING WARNING - THE gmst CALL HERE TOTALLY NERFS THE FUNCTION DOWN TO DOUBLE")
    val T_ut1 = JDtoJulianCenturyJ2000(julianDate)
    var gmst = T_ut1 * (T_ut1 * (T_ut1 * -6.2E-6 + 0.093104) + (876600 * Time.SEC_PER_HR + 8640184.812866)) + 67310.54841
    gmst = (gmst /% Time.SEC_PER_DAY)._2 / 240.0
    //gmst = Math.IEEEremainder(gmst.doubleValue, Time.SEC_PER_DAY) / 240.0
    if(gmst < 0.0) gmst+360.0 else gmst
  }

  /**
   * Compute the Modified Julian Date equivalent of this date  This function
   * is valid from 1900-2100.
   */
  def JDtoMJD(julianDate : BigDecimal) = julianDate - 2400000.5

  def jdnow = {
    val cal = Calendar.getInstance(TimeZone.getTimeZone("UTC"))
    gregorianDateToJD(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH)+1,
      cal.get(Calendar.DAY_OF_MONTH), cal.get(Calendar.HOUR_OF_DAY),
      cal.get(Calendar.MINUTE), cal.get(Calendar.SECOND),
      BigDecimal(cal.get(Calendar.MILLISECOND).doubleValue, java.math.MathContext.DECIMAL128))
  }

  def now = JD(jdnow)
}
