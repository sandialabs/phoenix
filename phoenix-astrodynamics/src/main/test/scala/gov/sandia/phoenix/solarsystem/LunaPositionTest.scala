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
