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

package gov.sandia.phoenix.time

import org.scalatest.FunSuite
import java.util.{TimeZone, GregorianCalendar, Calendar}
import scala.math.abs

class InstantImplementationsTest extends FunSuite {
  test("Default Constructor"){
    val now1 = now
    val cal1 = new GregorianCalendar(TimeZone.getTimeZone("UTC"))
    val g1 = now1.toGregorianDate
    assert(g1.year === cal1.get(Calendar.YEAR))
    assert(g1.monthOfYear === cal1.get(Calendar.MONTH) + 1)
    assert(g1.dayOfMonth === cal1.get(Calendar.DAY_OF_MONTH))
    assert(g1.hourOfDay === cal1.get(Calendar.HOUR_OF_DAY))
    assert(g1.minuteOfHour === cal1.get(Calendar.MINUTE))
    assert(g1.secondOfMinute === cal1.get(Calendar.SECOND))

    val now2 = now
    val cal2 = new GregorianCalendar(TimeZone.getTimeZone("UTC"))
    val g2 = now2.toGregorianDate
    assert(g2.year === cal2.get(Calendar.YEAR))
    assert(g2.monthOfYear === cal2.get(Calendar.MONTH) + 1)
    assert(g2.dayOfMonth === cal2.get(Calendar.DAY_OF_MONTH))
    assert(g2.hourOfDay === cal2.get(Calendar.HOUR_OF_DAY))
    assert(g2.minuteOfHour === cal2.get(Calendar.MINUTE))
    assert(g2.secondOfMinute === cal2.get(Calendar.SECOND))
  }

  test("Calendar Date Equality"){
    assert(JANUARY === JANUARY)
    var a = new CalendarDate(2008, JANUARY, 1)
    var b = new CalendarDate(2008, JANUARY, 1)
    assert(a === b)
    a = new CalendarDate(2008, JANUARY, 1)
    b = new CalendarDate(2008, JANUARY, 2)
    assert(a != b)
  }

  test("Same"){
    val t1000 = TimeBuilder(2006, 1, 1, 10, 0, 0, 0)
    assert(!t1000.isAfter(t1000))
    assert(!t1000.isBefore(t1000))
  }

  test("JulianDate Equality"){
    val a = TimeBuilder(2008, 1, 1, 1, 1, 1, 1.0)
    val b = TimeBuilder(2008, 1, 1, 1, 1, 1, 1.0)
    assert(a === b)
  }

  test("Calendar Date Conversion"){
    var t = TimeBuilder(2008, 1, 1)
    var g = t.toGregorianDate
    var cd = g.toCalendarDate
    assert(cd.month === JANUARY)
    t = TimeBuilder(2008, 1, 1)
    cd = g.toCalendarDate
    assert(cd.month === JANUARY)
    t = cd.toJulianDate
    assert(t.doubleValue === TimeBuilder(2008, 1, 1).doubleValue)
  }

  test("To Julian Date"){
    //January 31, 2007 == 2454131.5, Test is for last day of month.
    assert(TimeBuilder(2007, 1, 31, 0, 0, 0, 0).doubleValue === 2454131.5)
    //December 31, 2007 == 2454465.5, Test is for last day of year.
    assert(TimeBuilder(2007, 12, 31, 0, 0, 0, 0).doubleValue === 2454465.5)
    assert(TimeBuilder(1978, 7, 21, 15, 23, 0, 0).doubleValue === 2443711.140972222)
    assert(TimeBuilder(1978, 7, 21, 15, 23, 32, 0).doubleValue === 2443711.1413425924)
    assert(TimeBuilder(2007, 8, 31, 9, 0, 0, 0).doubleValue === 2454343.875)
    assert(TimeBuilder(2007, 8, 31, 9, 0, 12, 0).doubleValue === 2454343.875138889)
    assert(TimeBuilder(2007, 6, 1, 0, 0, 0, 0).doubleValue === 2454252.5)
    assert(TimeBuilder(2007, 6, 2, 0, 0, 0, 0).doubleValue === 2454253.5)
    assert(TimeBuilder(2008, 4, 30, 0, 0, 0, 0).doubleValue === 2454586.5)
    val months = Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    //January 1, 2007 - run through one whole year.
    var start = 2454101.5
    (0 until Months.MONTHS_IN_YEAR) foreach { month =>
      (1 to months(month)) foreach { day =>
        assert(TimeBuilder(2007, month + 1, day, 0, 0, 0, 0).doubleValue === start)
        start = start + 1
      }
    }
  }

  test("To Gregorian Date"){
    //January 1, 2007 == 2454101.5, Test is for first day of month.
    var t = TimeBuilder(2454101.5)
    var g = t.toGregorianDate
    assert(g.year == 2007)
    assert(g.monthOfYear == 1)
    assert(g.dayOfMonth == 1)

    //January 31, 2007 == 2454131.5, Test is for last day of month.
    t = TimeBuilder(2454131.5)
    g = t.toGregorianDate
    assert(g.year == 2007)
    assert(g.monthOfYear == 1)
    assert(g.dayOfMonth == 31)

    //December 31, 2007 == 2454465.5, Test is for last day of year.
    t = TimeBuilder(2454465.5)
    g = t.toGregorianDate
    assert(g.year == 2007)
    assert(g.monthOfYear == 12)
    assert(g.dayOfMonth == 31)

    //May 31, 2007 == 2454251.5  -- used to test previous failure mode.
    t = TimeBuilder(2007, 5, 31, 0, 0, 0, 0.0)
    g = t.toGregorianDate
    assert(t.doubleValue === 2454251.5)
    assert(g.year == 2007)
    assert(g.monthOfYear == 5)
    assert(g.dayOfMonth == 31)

    //June 1, 2007 == 2454252.5 -- Previous failure mode
    t = TimeBuilder(2007, 6, 1, 0, 0, 0, 0.0)
    g = t.toGregorianDate
    assert(t.doubleValue === 2454252.5)
    assert(g.year == 2007)
    assert(g.monthOfYear == 6)
    assert(g.dayOfMonth == 1)
    //January 1, 2007 == 2454101.5
    var d = 2454101.5
    val months = Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    (0 until Months.MONTHS_IN_YEAR) foreach { month =>
      (1 to months(month)) foreach { day =>
        t = TimeBuilder(2007, month + 1, day, 0, 0, 0, 0.0)
        g = t.toGregorianDate
        assert(t.doubleValue === d)
        d = d + 1
        assert(g.year == 2007)
        assert(g.monthOfYear == (month + 1))
        assert(g.dayOfMonth == day)
      }
    }
  }

  test("Constructor"){
    val months = Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    (0 until Months.MONTHS_IN_YEAR) foreach { month =>
      (1 to months(month)) foreach { day =>
        val time = TimeBuilder(2007, month + 1, day, 0, 0, 0, 0)
        val g = time.toGregorianDate
        assert(g.monthOfYear === month + 1)
        assert(g.dayOfMonth === day)
      }
    }
  }

  test("To TT"){
    val time0 = TimeBuilder(2006, 1, 1, 0, 0, 0, 0)
    val time1 = TimeBuilder(2005, 12, 31, 24, 0, 0, 0)
    val time2 = TimeBuilder(2005, 12, 31, 23, 59, 59, 1000.0)
    assert(time0.doubleValue === time2.doubleValue)
    assert(time0.doubleValue === time1.doubleValue)
    assert(time1.doubleValue === time2.doubleValue)

    //Aug 1 2007 8:49:15
    val time = TimeBuilder(2454313.867534722)
    val g = time.toGregorianDate
    assert(g.year === 2007)
    assert(g.monthOfYear === 8)
    assert(g.dayOfMonth === 1)
    assert(g.hourOfDay === 8)
    assert(g.minuteOfHour === 49)
    assert(abs(g.secondOfMinute + g.millisOfSecond / 1000.0 - 15) <= 1E-3)
  }

  test("toGreenwichMeanSiderealTime"){
    val t = TimeBuilder(1992, 8, 20, 12, 14, 0, 0.0)
    val gmst = t.toGMST
    assert(abs(152.578787886 - gmst.doubleValue) <= 1E-7)
  }

  test("toModifiedJulianDate"){
    val t = TimeBuilder(2007, 9, 30, 0, 0, 0, 0.0)
    val jd = t.doubleValue
    val mjd = t.toMJD.doubleValue
    assert((jd - 2400000.5 - mjd) <= 1E-5)
  }

  test("To Julian Century J2000"){
    val t = TimeBuilder(1992, 8, 20, 12, 14, 0, 0.0)
    val j2000 = t.toJulianCenturyJ2000
    assert(abs(-0.073647919 - j2000) <= 1E-7)
  }

  test("To Date"){
    val t = TimeBuilder(2007, 1, 31, 0, 0, 0, 0)
    val cal = new GregorianCalendar(TimeZone.getTimeZone("UTC"))
    cal.clear
    cal.set(2007, 0, 31)
    val d0 = t.toDate
    val d1 = cal.getTime
    assert(d0.getTime === d1.getTime)
  }
}
