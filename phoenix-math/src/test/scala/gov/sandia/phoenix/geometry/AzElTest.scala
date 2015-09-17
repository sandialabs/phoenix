package gov.sandia.phoenix.geometry

import org.scalatest.FunSuite

class AzElTest extends FunSuite {
  //Ascending non-crossover
  test("30 to 270") {
    val lo = new AzEl(30, 0)
    val hi = new AzEl(270, 0)
    val res = lo.interpolate(0.5, hi)
    assert(res.azimuth === 150)
  }

  //Descending non-crossover
  test("270 to 30") {
    val lo = new AzEl(270, 0)
    val hi = new AzEl(30, 0)
    val res = lo.interpolate(0.5, hi)
    assert(res.azimuth === 150)
  }

  //Descending with crossover
  test("30 to -90") {
    val lo = new AzEl(30, 0)
    val hi = new AzEl(-90, 0)
    val res = lo.interpolate(0.5, hi)
    assert(res.azimuth === -30)
  }

  //Ascending with crossover
  test("-90 to 30") {
    val lo = new AzEl(-90, 0)
    val hi = new AzEl(30, 0)
    val res = lo.interpolate(0.5, hi)
    assert(res.azimuth === -30)
  }


}
