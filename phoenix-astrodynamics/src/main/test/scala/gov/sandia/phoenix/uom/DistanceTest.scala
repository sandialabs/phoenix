package gov.sandia.phoenix.uom

import org.scalatest.FunSuite

class DistanceTest extends FunSuite {
  test("yards to feet") {
    assert(Yards(1) === Feet(3).yards)
    assert(Yards(5280) === Miles(3).yards)
    assert(Foot.inches == Inches(12))
    assert(Foot == Inches(12).feet)
  }
}