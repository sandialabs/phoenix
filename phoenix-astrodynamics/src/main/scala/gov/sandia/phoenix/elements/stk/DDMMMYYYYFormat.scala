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