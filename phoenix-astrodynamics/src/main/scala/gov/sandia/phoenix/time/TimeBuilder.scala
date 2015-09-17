package gov.sandia.phoenix.time

import scala.math.BigDecimal
import java.util.Date

object TimeBuilder {
  def apply(jd : Double) : JD = JD(BigDecimal(jd, java.math.MathContext.DECIMAL128))
  def apply(year : Int, monthOfYear : Int, dayOfMonth : Int, hourOfDay : Int,
           minuteOfHour : Int, secondOfMinute : Int, millisOfSecond : Double) : JD =
    JD(gregorianDateToJD(year, monthOfYear, dayOfMonth, hourOfDay,
      minuteOfHour, secondOfMinute, BigDecimal(millisOfSecond, java.math.MathContext.DECIMAL128)))
  def apply(year : Int, monthOfYear : Int, dayOfMonth : Int, hourOfDay : Int,
           minuteOfHour : Int, secondOfMinute : Int) : JD =
    apply(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, secondOfMinute, 0)
  def apply(year : Int, monthOfYear : Int, dayOfMonth : Int, hourOfDay : Int, minuteOfHour : Int) : JD =
    apply(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, 0)
  def apply(year : Int, monthOfYear : Int, dayOfMonth : Int, hourOfDay : Int) : JD = apply(year, monthOfYear, dayOfMonth, hourOfDay, 0)
  def apply(year : Int, monthOfYear : Int, dayOfMonth : Int) : JD = apply(year, monthOfYear, dayOfMonth, 0)
  def apply(instant : Date) : JD = JD(dateToJulianDate(instant))
}
