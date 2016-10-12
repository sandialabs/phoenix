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
sealed abstract case class Day(index : Int, name : String) {
  def next = Days.next(index)
  def previous = Days.previous(index)
  override def toString = name
  val abbreviation = name.substring(0, 3)
  def dayOfWeek = index + 1
}
object SUNDAY extends Day(0, "Sunday")
object MONDAY extends Day(1, "Monday")
object TUESDAY extends Day(2, "Tuesday")
object WEDNESDAY extends Day(3, "Wednesday")
object THURSDAY extends Day(4, "Thursday")
object FRIDAY extends Day(5, "Friday")
object SATURDAY extends Day(6, "Saturday")

object Days extends ArrayOfCyclicalEvents[Day](Vector(SUNDAY, MONDAY, TUESDAY, WEDNESDAY, 
        THURSDAY, FRIDAY, SATURDAY)) {
  val DAYS_IN_WEEK = count
}