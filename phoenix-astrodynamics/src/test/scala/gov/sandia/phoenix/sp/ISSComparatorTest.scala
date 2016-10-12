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

package gov.sandia.phoenix.sp

import gov.sandia.phoenix.elements.tle.TLE
import gov.sandia.phoenix.geometry.Vector3
import gov.sandia.phoenix.propagators.sgp4.SGP4
import org.scalatest.FunSuite

class ISSComparator extends FunSuite {
  test("Comparison of Force Propagation to a TLE") {
    val name = "ISS (ZARYA)"
    val l1 = "1 25544U 98067A   15069.16943431  .00017956  00000-0  26895-3 0  9993"
    val l2 = "2 25544 051.6451 218.6166 0008831 088.1115 091.0199 15.54993621932624"
    val tle = TLE(Some(name), l1, l2)
    val prop = SGP4(tle, true)
    val start = tle.epoch
    val finish = start plusDays 1
    val interval = start until finish
    val initialState = prop.unsafe_state(start)

    //Sun, Moon, Earth + Solar Radiation Pressure
    val forceModel = SMGModel+SRP()

    for(s <- new FP(interval.start, initialState, forceModel).solve(interval.end)) {
      val t = start plusSeconds s._1
      val act = prop.unsafe_state(t).position
      val est = Vector3(s._2(0), s._2(1), s._2(2))
      //Turn this back on to see actual errors.
//      println("t:   " + t)
//      println("ACT: " + act)
//      println("EST: " + est)
//      println("DIF: " + (est - act).mag)
      //Assert that the difference is never less than 2 km.
      assert((est - act).mag < 5000)
    }
  }
}
