package gov.sandia.phoenix.uom

import org.scalatest.FunSuite

class SpeedTest extends FunSuite {
  test("knots"){
    assert(Knots(1).kilometersPerHour === KilometersPerHour(1.852))
  }

  test("invertible"){
    assert(Knots(1).kilometersPerHour.knots === Knots(1))
    assert(KilometersPerHour(1).knots.kilometersPerHour === KilometersPerHour(1))
  }

  test("user defined"){
    assert(UserDefinedSpeed(Kilometers(2), Seconds(10)).metersPerSecond === MetersPerSecond(200))
    assert((Kilometers(2) per Second).metersPerSecond === MetersPerSecond(2000))
  }

  test("knots->X"){
    assert(Knots(35).metersPerSecond === MetersPerSecond(35.0 * 1852 / 3600))
    assert(Knots(35).kilometersPerHour === KilometersPerHour(35.0 * 1.852))
    assert(Knots(35).feetPerSecond === FeetPerSecond(59.07334499854185))
    assert(Knots(35).milesPerHour === MilesPerHour(40.277280680823985))
  }
}