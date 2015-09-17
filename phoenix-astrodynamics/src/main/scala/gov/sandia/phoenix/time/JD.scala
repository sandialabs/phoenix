package gov.sandia.phoenix.time

import gov.sandia.phoenix.constants._
import gov.sandia.phoenix.eop.EmptyEOPEntry
import gov.sandia.phoenix.geometry.{Geodetic, Vector3}
import gov.sandia.phoenix.transforms.FK5

import scala.math._

/**
 * A class that conveniently wraps a JulianDate.
 *
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class JD(value : BigDecimal) extends Ordered[JD] {
  //To other common formats
  def toGregorianDate = JDtoGregorianDate(value)
  def toGMST = GMST(JDtoGMST(value))
  def toJulianCenturyJ2000 = JDtoJulianCenturyJ2000(value).doubleValue
  def toMJD = JDtoMJD(value).doubleValue
  def toJulianCentury(epoch : JD) = (value - epoch.value) / 36525
  def toDate = toGregorianDate.toDate

  def min(that : JD) = if(this.value < that.value) this else that
  def max(that : JD) = if(this.value > that.value) this else that

  def doubleValue = this.value.doubleValue

  //Comparators
  def isAfter(time : JD) = this > time
  def isBefore(time : JD) = this < time
  def isEqual(time : JD) = this == time

  //Offsetters
  def plusWeeks(weeks : Double) : JD = JD(value + weeks * Time.DAY_PER_WEEK)
  def plusDays(days : Double) : JD = JD(value + days)
  def plusHours(hours : Double) : JD = JD(value + BigDecimal(hours, java.math.MathContext.DECIMAL128) / Time.HR_PER_DAY)
  def plusMinutes(minutes : Double) : JD = JD(value + BigDecimal(minutes, java.math.MathContext.DECIMAL128) / Time.MIN_PER_DAY)
  def plusSeconds(seconds : Double) : JD = JD(value + BigDecimal(seconds, java.math.MathContext.DECIMAL128) / Time.SEC_PER_DAY)
  def plusMillis(millis : Double) : JD = JD(value + BigDecimal(millis, java.math.MathContext.DECIMAL128) / Time.MS_PER_DAY)
  def minusWeeks(weeks : Double) : JD = JD(value - weeks * Time.DAY_PER_WEEK)
  def minusDays(days : Double) : JD = JD(value - days)
  def minusHours(hours : Double) : JD = JD(value - BigDecimal(hours, java.math.MathContext.DECIMAL128) / Time.HR_PER_DAY)
  def minusMinutes(minutes : Double) : JD = JD(value - BigDecimal(minutes, java.math.MathContext.DECIMAL128) / Time.MIN_PER_DAY)
  def minusSeconds(seconds : Double) : JD = JD(value - BigDecimal(seconds, java.math.MathContext.DECIMAL128) / Time.SEC_PER_DAY)
  def minusMillis(millis : Double) : JD = JD(value - BigDecimal(millis, java.math.MathContext.DECIMAL128) / Time.MS_PER_DAY)
  def plusDay = plusDays(1)
  def plusSiderealDay = plusDays(Time.EARTH_MEAN_SIDEREAL_DAY)

  def since(epoch : JD) = Interval(epoch, this)
  def sinceGPS = since(GPS_EPOCH)
  def sinceUNIX =  since(UNIX_EPOCH)
  def mid(that : JD) = JD((this.value + that.value) * 0.5)
  def until(that : JD) = Interval(this, that)
  def dayInterval(days : Double) = this until (this plusDays days)
  def secondInterval(seconds : Double) = this until (this plusSeconds seconds)
  /** Compute the time delta (in seconds) between two Instants */
  def Δ(that : JD) = (this.value - that.value) * Time.SEC_PER_DAY_BD
  /** Compute the time delta (in seconds) between two Instants */
  def delta(that : JD) = this Δ that

  def getDayOfWeek = (doubleValue + 1.5).intValue % 7
  def getDayOfYear = doubleValue - TimeBuilder(toGregorianDate.year, 1, 0).doubleValue

  override def compare(that : JD) = this.value.compare(that.value)

  def floor = value /% 1 match {
    case (whole, rem) if rem > 0.5 => JD(whole + 0.5)
    case (whole, rem) if rem < 0.5 => JD(whole - 0.5)
    case _ => this
  }

  def ceil = value /% 1 match {
    case (whole, rem) if rem > 0.5 => JD(whole + 1.5)
    case (whole, rem) if rem < 0.5 => JD(whole + 0.5)
    case _ => this
  }

  //TODO: Re-evaluate whether or not to use a fixed EmptyEOPEntry.DAT value for "none". Wouldn't it be better to use TAI(this)?
  lazy val fk5 = FK5.eopType match {
    case "stk" => FK5.applySTKEOP(this)
    case "celestrak" => FK5.applySTKEOP(this)
    case "none" => new FK5(this, EmptyEOPEntry.dUT1, EmptyEOPEntry.DAT, EmptyEOPEntry.x, EmptyEOPEntry.y, EmptyEOPEntry.LOD)
    case _ => new FK5(this, EmptyEOPEntry.dUT1, EmptyEOPEntry.DAT, EmptyEOPEntry.x, EmptyEOPEntry.y, EmptyEOPEntry.LOD)
  }

  //Convenience methods to convert from one frame to another.
  def TEMEtoECI(teme : Vector3) = fk5.TEMEtoJ2000(teme)
  def ECEFtoECI(ecef : Vector3) = fk5.ITRFtoJ2000(ecef)
  def GEOtoECI(geo : Geodetic) = ECEFtoECI(geo.toECEF)
  def ECItoECEF(eci : Vector3) = fk5.J2000toITRF(eci)
  def ECItoGEO(eci : Vector3) = ECItoECEF(eci).toGeodetic
  def ECItoTOD(eci : Vector3) = fk5.J2000toTOD(eci)

  def toLocalSiderealTime(longitude : Double) = {
    val lst = math.IEEEremainder(toGMST.doubleValue + longitude, 360.0)
    if(lst < 0.0) lst + 360.0 else lst
  }

  def toGreenwichMeanSiderealTime0Hour = {
    val t_ut = toJulianCenturyJ2000
    IEEEremainder(100.4606184 + (36000.77005361 + (0.00038793 - 2.6E-8 * t_ut) * t_ut) * t_ut, 360.0)
  }

  def toGPSWeek = (GPS_EPOCH until this).getDurationSeconds / Time.SEC_PER_WEEK

  def toTAI = this plusSeconds TAI(this)
  def toTT = this plusSeconds TT(this)
  def toTDB = {
    val tt = this.toTT
    def M(Ttt : Double) = IEEEremainder(357.5277233 + 35999.05034*Ttt, 360)
    val meanAnomaly = M(tt.toJulianCenturyJ2000).toRadians
    tt plusSeconds (0.001658 * sin(meanAnomaly) + 0.00001385 * sin(2.0 * meanAnomaly))
  }
  def toGPS = this plusSeconds GPS(this)

  /**
   * return the mean longitude, mean anomaly, ecliptic latitude, and obliquity
   * of the ecliptic for the Sun. All values are in radians.
   */
  def solarParams = {
    val t_ut1 = toJulianCenturyJ2000
    val λ = (280.4606184 + 36000.77005361 * t_ut1) % 360.0
    val M = ((357.5277233 + 35999.05034 * t_ut1) % 360.0).toRadians
    val λ_ecliptic = λ + 1.914666471 * sin(M) + 0.019994643 * sin(2.0 * M)
    val ε = (23.439291 - 0.0130042 * t_ut1) % 360.0
    (λ.toRadians, M, λ_ecliptic.toRadians, ε.toRadians)
  }
}