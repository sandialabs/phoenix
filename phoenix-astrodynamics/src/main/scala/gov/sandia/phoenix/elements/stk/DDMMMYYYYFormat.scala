package gov.sandia.phoenix.elements.stk

import java.text.DecimalFormat

import gov.sandia.phoenix.time.{Months, GregorianDate, JD}

object DDMMMYYYYFormat {
  def apply(o : Any) : String = o match {
    case GregorianDate(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, secondOfMinute, millisOfSecond) =>
      val df4 = new DecimalFormat("0000")
      val df2 = new DecimalFormat("00")
      val dfX = new DecimalFormat("00.###########################")
      val millis = (millisOfSecond * 1E15).longValue / 1E15

      df2.format(dayOfMonth) + " " + Months(monthOfYear - 1) + " " +
        df4.format(year) + " " + df2.format(hourOfDay) + ":" +
        df2.format(minuteOfHour) + ":" + dfX.format(secondOfMinute + millis / 1000.0)
    case jd : JD => this(jd.toGregorianDate)
    case _ => o.toString
  }
}