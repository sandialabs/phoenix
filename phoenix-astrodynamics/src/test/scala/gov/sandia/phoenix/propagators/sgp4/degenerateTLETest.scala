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

package gov.sandia.phoenix.propagators.sgp4



import org.scalatest.FunSuite

import gov.sandia.phoenix.time._
import gov.sandia.phoenix.elements.tle.TLE

/**
 * Ensure that phoenix correctly handles tles in a decayed state. For sample decayed TLEs, go to
 * https://www.space-track.org/#/decay and set the propagation epoch to one far beyond the decay epoch.
 */

class degenerateTLETest extends FunSuite {
  test("Cosmos Debris - Calling SGP4") {
    val l0 = "COSMOS 2251 DEB"
    val l1 = "1 37116U 93036BGN 12256.29631138  .00060717  00000-0  37173-2 0   915"
    val l2 = "2 37116 073.8830 337.8463 0097317 341.7198 018.1004 15.06161973124205"
    val tle = TLE(Some(l0), l1, l2)
    val sgp4 = SGP4(tle)

    val t = TimeBuilder(2015, 1 , 1)

    sgp4.state(t) match {
      case Some(state) => println(state)
      case None => assert(true, "Success!.")
    }
  }
}
