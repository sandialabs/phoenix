package gov.sandia.phoenix.core.conversions

import gov.sandia.phoenix.constants.WGS84
import gov.sandia.phoenix.geometry._
import org.scalatest.FunSuite

import scala.math._

class GeoFrameConversionsTest extends FunSuite {
  test("North Pole Conversion : ECEF -> Geodetic") {
    val ecef = new Vector3(0.0, 0.0, WGS84.R_POLAR_M)
    val geodetic = ecef.toGeodetic
    assert(abs(geodetic.longitude) < java.lang.Float.MIN_VALUE)
    assert(abs(geodetic.latitude - 90.0) < java.lang.Float.MIN_VALUE)
    assert(abs(geodetic.elevation) < java.lang.Float.MIN_VALUE)
  }
  
  test("South Pole Conversion : ECEF -> Geodetic") {
    val ecef = new Vector3(0.0, 0.0, -WGS84.R_POLAR_M)
    val geodetic = ecef.toGeodetic
    assert(abs(geodetic.longitude) < java.lang.Float.MIN_VALUE)
    assert(abs(geodetic.latitude + 90.0) < java.lang.Float.MIN_VALUE)
    assert(abs(geodetic.elevation) < java.lang.Float.MIN_VALUE)
  }
  
  test("Equator Conversion : ECEF -> Geodetic") {
    val ecef = new Vector3(WGS84.R_EQ_M, 0.0, 0.0)
    val geodetic = ecef.toGeodetic
    assert(abs(geodetic.longitude) < java.lang.Float.MIN_VALUE)
    assert(abs(geodetic.latitude) < java.lang.Float.MIN_VALUE)
    assert(abs(geodetic.elevation) < java.lang.Float.MIN_VALUE)
  }
  
  test("Equator Conversion : Geodetic -> ECEF") {
    val geodetic = Geodetic(0.0, 0.0, 0.0)
    val ecef = geodetic.toECEF
    assert(abs(ecef.x - WGS84.R_EQ_M) < java.lang.Float.MIN_VALUE)
    assert(abs(ecef.y) < java.lang.Float.MIN_VALUE)
    assert(abs(ecef.z) < java.lang.Float.MIN_VALUE)
  }
  
  test("Test both directions") {
    //Create a series of geodetic->ecef->geodetic conversion
    for(lat <- -90.0 to 90.0 by 5.0; lon <- -180.0 to 180.0 by 5.0; elv <- -1000.0 to 1000.0 by 5.0) {
      val geodeticStart = Geodetic(lon, lat, elv)
      val ecef = geodeticStart.toECEF
      val geodetic = ecef.toGeodetic
      assert(abs(geodetic.longitude - lon) < 1E-9)
      assert(abs(geodetic.latitude - lat) < 1E-9)
      assert(abs(geodetic.elevation - elv) < 1E-8)
    }
  }
}
