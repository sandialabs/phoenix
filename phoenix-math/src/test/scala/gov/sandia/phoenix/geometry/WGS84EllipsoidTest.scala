package gov.sandia.phoenix.geometry

import gov.sandia.phoenix.constants.WGS84
import org.scalatest.FunSuite

class WGS84EllipsoidTest extends FunSuite
{
  test("intersectNadir")
  {
    val ecef = new Vector3(WGS84.R_EQ_M * 2, WGS84.R_EQ_M * 2, WGS84.R_EQ_M * 2)
    val dir = -ecef.normalized
    val ray = new Ray(ecef, dir)
    val intersects = WGS84.ellipsoid.intersect(ray)
    assert(intersects.size === 2)
  }

  test("intersectExample") {
    val p = new Vector3(WGS84.R_EQ_M * 2, 0, 0)
    val ray = new Ray(p, p.normalized.negated)
    val intersects = WGS84.ellipsoid.closestIntersection(ray)
    val ans = new Vector3(WGS84.R_EQ_M, 0, 0)
    assert(ans close intersects)
  }
}