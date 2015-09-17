package gov.sandia.phoenix.time

import java.text.DecimalFormat

/**
 * A calendar date is simply a year, month, and day with no precision added for
 * time of day.  If you look at any wall calendar, it makes no reference to
 * hours, minutes, seconds, time zones, etc.  Unfortunately, many of the
 * calendar widgets out there in the wild have time fields, which make them
 * a real pain to use when you have to do things like day rollover, handle
 * multiple time zones, etc.
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class CalendarDate(year : Int, month : Month, day : Int) extends Ordered[CalendarDate] {
  def toJulianDate = TimeBuilder(year, month.monthOfYear, day)

  override def toString = new DecimalFormat("00").format(day) + " " + month + " " + year

  def dayOfWeek = month.getDayOfWeek(day, year)
  
  def tomorrow = {
    val days = month.getDays(year)
    if(day == days) {
      val nextMonth = month.next
      new CalendarDate(if(nextMonth == JANUARY) year + 1 else year, nextMonth, 1)
    }
    else new CalendarDate(year, month, day+1)
  }
  
  def yesterday = if(day == 1) {
    val previousMonth = month.previous
    val newYear = if(previousMonth == DECEMBER) year - 1 else year
    new CalendarDate(newYear, previousMonth, previousMonth.getDays(newYear))
  } else new CalendarDate(year, month, day-1)
  
  def next = tomorrow
  def previous = yesterday

  def compare(that : CalendarDate) = {
    val ydiff = this.year - that.year
    if(ydiff != 0) ydiff else {
      val mdiff = this.month.index - that.month.index
      if(mdiff != 0) mdiff else {
        this.day - that.day
      }
    }
  }
}