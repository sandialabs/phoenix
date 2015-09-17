package gov.sandia.phoenix.geometry

import gov.sandia.phoenix.constants.WGS84
import org.scalatest.FunSuite

class WGS84SphereTest extends FunSuite
{
  test("intersectNadir") {
    val ecef = new Vector3(WGS84.R_EQ_M * 2, WGS84.R_EQ_M * 2, WGS84.R_EQ_M * 2)
    val dir = -ecef.normalized
    val ray = new Ray(ecef, dir)
    val intersects = WGS84.sphere.intersect(ray)
    assert(intersects.size === 2)
  }

  test("miss") {
    val p = new Vector3(2*WGS84.sphere.radius, 2*WGS84.sphere.radius, 2*WGS84.sphere.radius)
    val dir = new Vector3(0, 0, -1)
    val ray = new Ray(p, dir)
    val intersects = WGS84.sphere.intersect(ray)
    assert(intersects.size == 0)
    assert(WGS84.sphere.closestIntersection(ray) === null)
  }

  test("sphere-plane") {
    val p = new Plane(WGS84.R_EQ_M * 0.5, X_AXIS)
    val intersects = WGS84.sphere.intersect(p)
    assert(p == intersects.center)
  }
}