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