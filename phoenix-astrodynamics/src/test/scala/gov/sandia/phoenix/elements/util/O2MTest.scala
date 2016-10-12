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

package gov.sandia.phoenix.elements.util

import gov.sandia.phoenix.elements.tle.TLE
import gov.sandia.phoenix.propagators.sgp4.SGP4
import org.scalatest.FunSuite

class O2MTest extends FunSuite {
  test("MOLNIYA 1-93") {
    test("MOLNIYA 1-93",
      "1 28163U 04005A   13065.25189137  .00000070  00000-0  10000-3 0   802",
      "2 28163  64.7233 136.1772 7053508 252.6309  33.6837  2.00638435 66282", 1.0E-8, 10)
  }

  test("DIRECTV 12") {
    test("DIRECTV 12",
      "1 36131U 09075A   14198.55284238 -.00000123  00000-0  00000+0 0  2398",
      "2 36131   0.0029 182.0214 0000262 274.3669 295.1389  1.00271404 16746", 1.0, 100)
  }

  def test(name : String, l1 : String, l2 : String, tol : Double, steps : Int) = {
    val tle = TLE(Some(name), l1, l2)
    val osculating = SGP4(tle).unsafe_state(tle.epoch)
    val o2m = new OsculatingToMeanElements(osculating, tle.epoch, tle.bstar)
    o2m.solve(tol, steps) match {
      case Some(mean) =>
        val deltas = o2m.fx(mean)
        assert(deltas.position.mag <= 1.0E-6, "Sub-micrometer tolerance not met.")
        assert(deltas.velocity.mag <= 1.0E-6, "Sub-micrometer/s tolerance not met.")
      case None => assert(false, "This should have converged.")
    }
  }
}