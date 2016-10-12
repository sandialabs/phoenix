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

package gov.sandia.phoenix.access

import gov.sandia.phoenix.constants.{Time, WGS84}
import gov.sandia.phoenix.elements.tle._
import gov.sandia.phoenix.geometry._
import gov.sandia.phoenix.propagators.sgp4.SGP4
import gov.sandia.phoenix.time._
import org.scalatest.FunSuite

import scala.math._


class AccessesTests extends FunSuite
{
  test("ISS Passes 20081113"){
    //Information for this test was taken from www.heavens-above.com
    val tle = TLE(None,
        "1 25544U 98067A   08318.22101670  .00011943  00000-0  91629-4 0  6412",
          "2 25544  51.6441 332.4033 0003785 347.9711  88.1728 15.72573517571983")
    val propagator = SGP4(tle)

    val ecef = Geodetic(0.0, 0.0, 0.0).toECEF

    val start = TimeBuilder(2008, 11, 13, 18, 43, 9)
    val a = start minusMinutes 45
    val b = start plusMinutes 45
    val interval = a until b
    Accesses.continuous(interval, 300, 1){ t =>
      Access(ecef, t.ECItoECEF(propagator.unsafe_position(t)), Degrees(10.0))
    } find { _.contains(start) } foreach { pass =>
      val passStart = TimeBuilder(2008, 11, 13, 18, 40, 24)
      val startDiff = abs((passStart.toMJD - pass.start.toMJD) * Time.MIN_PER_DAY)
      assert(startDiff < 1.0)
    }

    Accesses.continuous(interval, 300, 1){ t =>
      Access(ecef, t.ECItoECEF(propagator.unsafe_position(t)), Degrees(15.0))
    } find { pass => pass.contains(start)
    } foreach { pass =>
      val passStop = TimeBuilder(2008, 11, 13, 18, 45, 12)
      val stopDiff = abs((passStop.toMJD - pass.end.toMJD) * Time.MIN_PER_DAY)
      assert(stopDiff < 2.0)
    }
  }

  test("Molniya-3 Passes 20081113")
  {
    //Information for this test was taken from www.heavens-above.com
    val tle = new TLE(None,
      "1 20813U 90084A   08313.15127079  .00000973  00000-0 -33732-3 0   703",
      "2 20813  62.2032  99.6261 7462822 266.7623  14.3573  2.00574962132865")
    val propagator = SGP4(tle)

    val ecef = Geodetic(0.0, 0.0, 0.0).toECEF

    //Pass times
    val start = TimeBuilder(2008, 11, 13)
    val interval = Interval(start, start.plusDays(10))

    //val passes = poi.computePasses(sat, interval, 10.0)
    val sampledAccesses = Accesses.sample(interval, 60 * 5){ t =>
      Access(ecef, t.ECItoECEF(propagator.unsafe_position(t)), Degrees(0.0))
    }

    val continuousAccesses = Accesses.samplesToContinuous(sampledAccesses, 1){ t =>
      Access(ecef, t.ECItoECEF(propagator.unsafe_position(t)), Degrees(0.0))
    }

    assert(continuousAccesses.size === 10)
  }
}