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
