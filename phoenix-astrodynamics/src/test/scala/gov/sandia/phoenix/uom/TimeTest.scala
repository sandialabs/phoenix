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

package gov.sandia.phoenix.uom

import org.scalatest.FunSuite

class TimeTest extends FunSuite {
  test("weeks") {
    assert(Weeks(1).seconds.weeks === Seconds(7 * 24 * 60 * 60).weeks)
  }

  test("conversions") {
    assert(Seconds(1).minutes.hours.days.weeks.seconds === Seconds(1))
    assert(Weeks(1).hours.weeks === Weeks(1))
    assert(Weeks(1).days === Days(7))
  }

  test("back and forth") {
    assert(Weeks(1) === Weeks(1).days.hours.minutes.seconds.weeks)
    assert(Seconds(1) === Seconds(1).minutes.hours.days.weeks.seconds)
  }
}