package gov.sandia.phoenix.time

import java.util.{Calendar, TimeZone, GregorianCalendar}

/**
 * A structure for storing time data with integer precision down to millis.
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class GregorianDate(year : Int, monthOfYear : Int, dayOfMonth : Int, hourOfDay : Int, minuteOfHour : Int,
                      secondOfMinute : Int, millisOfSecond : Double) {
  def toDate = {
    val cal = new GregorianCalendar
    cal.setTimeZone(TimeZone.getTimeZone("UTC"))
    cal.set(Calendar.YEAR, year)
    cal.set(Calendar.MONTH, monthOfYear-1)
    cal.set(Calendar.DAY_OF_MONTH, dayOfMonth)
    cal.set(Calendar.HOUR_OF_DAY, hourOfDay)
    cal.set(Calendar.MINUTE, minuteOfHour)
    cal.set(Calendar.SECOND, secondOfMinute)
    cal.set(Calendar.MILLISECOND, math.round(millisOfSecond).intValue)
    cal.getTime
  }

  def toCalendarDate = new CalendarDate(year, Months(monthOfYear-1), dayOfMonth)

  def toJulianDate = gregorianDateToJD(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, secondOfMinute, millisOfSecond)
}