package gov.sandia.phoenix.geometry

import org.scalatest.FunSuite

class AzElsTest extends FunSuite {
  test("angular sum that winds clockwise about zenith"){
    val azels = IndexedSeq(AzEl(359, 15), AzEl(1, 15), AzEl(180, 15))
    assert(AzEls.angularSum(azels) === 360)
    assert(AzEls.angularSum(azels.reverse) === -360)
    assert(AzEls.dividesPoles(azels))
  }

  test("non polar - northern and southern"){
    val azels = IndexedSeq(AzEl(-45, -20), AzEl(45, -20), AzEl(45, 20), AzEl(-45, 20))
    assert(AzEls.angularSum(azels) === 0)
    assert(AzEls.angularSum(azels.reverse) === 0)
  }

  test("north polar - positive azimuthal winding"){
    val azels = IndexedSeq(AzEl(0, 60), AzEl(90, 60), AzEl(180, 60), AzEl(270, 60))
    assert(AzEls.angularSum(azels) === 360)
    assert(AzEls.dividesPoles(azels))
  }

  test("north polar - negative azimuthal winding"){
    val azels = IndexedSeq(AzEl(270, 60), AzEl(180, 60), AzEl(90, 60), AzEl(0, 60))
    assert(AzEls.angularSum(azels) === -360)
    assert(AzEls.dividesPoles(azels))
  }

  test("south polar - positive azimuthal winding"){
    val azels = IndexedSeq(AzEl(0, -60), AzEl(90, -60), AzEl(180, -60), AzEl(270, -60))
    assert(AzEls.angularSum(azels) === 360)
    assert(AzEls.dividesPoles(azels))
  }

  test("south polar - negative azimuthal winding"){
    val azels = IndexedSeq(AzEl(270, -60), AzEl(180, -60), AzEl(90, -60), AzEl(0, -60))
    assert(AzEls.angularSum(azels) === -360)
    assert(AzEls.dividesPoles(azels))
  }

  test("both poles (or none) - negative azimuthal winding"){
    val azels = IndexedSeq(AzEl(0, 60), AzEl(90, 60), AzEl(180, 60), AzEl(270, 60),
      AzEl(270, -60), AzEl(180, -60), AzEl(90, -60), AzEl(0, -60))
    assert(AzEls.angularSum(azels) === 0)
    assert(AzEls.angularSum(azels.reverse) === 0)
    assert(!AzEls.dividesPoles(azels))
  }
}
