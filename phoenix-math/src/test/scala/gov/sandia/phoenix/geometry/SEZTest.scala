package gov.sandia.phoenix.geometry

import org.scalatest.FunSuite


class SEZTest extends FunSuite
{
  test("straightUp") {
    val location = Geodetic(0.0, 45.0, 1000)
    val ecef = location.toECEF
    val above = ecef * 1000.0
    val sez = location.toSEZFrame
    val azelr = sez.toAzElR(above)
    //This won't be exactly 0 because the SEZ frame uses an ellipsoid to compute the frame.
    assert(90.0 - azelr.elevation < 0.2)
  }

  test("straightUp @ (0,0,0)") {
    val location = Geodetic(0, 0, 0)
    val ecef = location.toECEF
    val above = ecef * 1000.0
    val sez = location.toSEZFrame
    val azelr = sez.toAzElR(above)
    //This won't be exactly 0 because the SEZ frame uses an ellipsoid to compute the frame.
    assert(90.0 - azelr.elevation < 0.2)
  }
}