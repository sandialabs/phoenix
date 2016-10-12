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

import java.util.logging.Logger

import gov.sandia.phoenix.constants.Time

import scala.collection.SortedSet

object IntervalUtil extends Ordering[Interval] {
  val logger = Logger.getLogger(getClass.getName)

  def siderealDayFrom(t : JD) = t until (t plusDays Time.EARTH_MEAN_SIDEREAL_DAY)
  def calendarDayFrom(t : JD) = t until (t plusDays 1)
  //Should this go in the companion?
  override def compare(x : Interval, y : Interval) = x compareTo y

  def today = calendarDayFrom(now.toGregorianDate.toCalendarDate.toJulianDate)

  def merge(intervals : Traversable[Interval]) = if(intervals.isEmpty) SortedSet.empty[Interval] else {
    val l = intervals.toList.sorted
    def m(interval : Interval, rest : List[Interval], res : SortedSet[Interval]) : SortedSet[Interval] = rest match {
      case Nil => res + interval
      case head :: tail => interval join head match {
        case Some(newInterval) => m(newInterval, tail, res)
        case None => m(head, tail, res + interval)
      }
    }
    m(l.head, l.tail, SortedSet.empty)
  }

  def dayFromNow = {
    val t = now
    Interval(t, t plusDays 1)
  }
}