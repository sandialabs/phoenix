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



/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
sealed abstract case class Month(index : Int,
                                 protected val days : Int,
                                 name : String) {
  def getDays(year : Int) : Int = days
  def days(year : Int) = getDays(year)
  def next = Months.next(index)
  def previous = Months.previous(index)
  override def toString = name
  val abbreviation = name.substring(0, 3)
  def abbr = abbreviation
  def monthOfYear = index + 1
  def getDayOfWeek(dayOfMonth : Int, year : Int) = 
      Days(TimeBuilder(year, monthOfYear, dayOfMonth).getDayOfWeek)
  def getFirstDay(year : Int) = getDayOfWeek(1, year)
}

object JANUARY extends Month(0, 31, "January")
object FEBRUARY extends Month(1, 28, "February") {
  override def getDays(year : Int) = 
      if(((year % 4 == 0) && (year % 100 != 0)) || (year % 400 == 0)) (days+1) else days
}
object MARCH extends Month(2, 31, "March")
object APRIL extends Month(3, 30, "April")
object MAY extends Month(4, 31, "May")
object JUNE extends Month(5, 30, "June")
object JULY extends Month(6, 31, "July")
object AUGUST extends Month(7, 31, "August")
object SEPTEMBER extends Month(8, 30, "September")
object OCTOBER extends Month(9, 31, "October")
object NOVEMBER extends Month(10, 30, "November")
object DECEMBER extends Month(11, 31, "December")

object Months extends ArrayOfCyclicalEvents[Month](Vector(JANUARY, FEBRUARY, MARCH, APRIL, 
        MAY, JUNE, JULY, AUGUST, SEPTEMBER, OCTOBER, NOVEMBER, DECEMBER)) {
  val MONTHS_IN_YEAR = count
}