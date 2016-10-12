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

package gov.sandia.phoenix.solarsystem

import gov.sandia.phoenix.constants.WGS84
import gov.sandia.phoenix.geometry.Vector3
import gov.sandia.phoenix.time.TimeBuilder
import org.scalatest.FunSuite

class LunaPositionTest extends FunSuite {
  /**
   * Test values taken from Vallado example 5-3.
   */
  test("ER Position at T = 1994/4/28") {
    val t = TimeBuilder(1994, 4, 28)
    val pos = Luna.positionER(t)
    val truth = Vector3(-21.0470851, -48.8499044, -19.8637462)
    val distER = pos dist truth
    val distMeters = distER * WGS84.R_EQ_M
    assert(distMeters <= 1.0)
  }
}
