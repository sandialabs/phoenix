package gov.sandia.phoenix.geometry

import org.scalatest.FunSuite

class AngleTest extends FunSuite {

  test("delta") {
    assert(Degrees(90) - Degrees(88) == Degrees(2))
    assert(Degrees(88) - Degrees(90) == Degrees(-2))
    assert(Degrees(270) - Degrees(2) == Degrees(268))
    assert(Degrees(2) - Degrees(270) == Degrees(-268))
  }

  test("constrain signed") {
    assert((Degrees(90) - Degrees(88)).constrainSigned == Degrees(2))
    assert((Degrees(88) - Degrees(90)).constrainSigned == Degrees(-2))
    assert((Degrees(270) - Degrees(2)).constrainSigned == Degrees(-92))
    assert((Degrees(2) - Degrees(270)).constrainSigned == Degrees(92))
  }

  test("constrain unsigned") {
    assert((Degrees(90) - Degrees(88)).constrainUnsigned == Degrees(2))
    assert((Degrees(88) - Degrees(90)).constrainUnsigned == Degrees(358))
    assert((Degrees(270) - Degrees(2)).constrainUnsigned == Degrees(268))
    assert((Degrees(2) - Degrees(270)).constrainUnsigned == Degrees(92))
  }
}
