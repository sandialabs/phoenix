package gov.sandia.phoenix.geometry

import gov.sandia.phoenix.constants.WGS84
import org.scalatest.FunSuite

import scala.math._

class GeodeticVsEllipsoidComparisonTest extends FunSuite {
  test("Equator") {
    val geo = Geodetic(0, 0, 0)
    val ecef = geo.toECEF
    val outer = ecef * 2
    val ray = new Ray(outer, (ecef - outer).normalized)
    val p = WGS84.ellipsoid.closestIntersection(ray)
    val geo2 = p.toGeodetic
    assert(abs(geo2.elevation) <= 1E-9)
  }
  
  test("Pole") {
    val geo = Geodetic(0, 90, 0)
    val ecef = geo.toECEF
    val outer = ecef * 2
    val ray = new Ray(outer, (ecef - outer).normalized)
    val p = WGS84.ellipsoid.closestIntersection(ray)
    val geo2 = p.toGeodetic
    assert(abs(geo2.elevation) <= 1E-9)
  }
  
  test("Mid-Latitude") {
    val geo = Geodetic(0, 45, 0)
    val ecef = geo.toECEF
    val outer = ecef * 2
    val ray = new Ray(outer, (ecef - outer).normalized)
    val p = WGS84.ellipsoid.closestIntersection(ray)
    val geo2 = p.toGeodetic
    assert(abs(geo2.elevation) <= 1E-9)
  }
}
