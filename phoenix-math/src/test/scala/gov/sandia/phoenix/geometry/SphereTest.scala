package gov.sandia.phoenix.geometry

import org.scalatest.FunSuite
import scala.math._

class SphereTest extends FunSuite
{
  test("hit") {
    val p = Vector3(10, 0, 0)
    val dir = Vector3(-1, 0, 0)
    val ray = Ray(p, dir)
    val intersects = UNIT_SPHERE.intersect(ray)
    assert(intersects.length == 2)
    val ans = Vector3(1, 0, 0)
    assert(UNIT_SPHERE.closestIntersection(ray) == ans)
  }
  
  test("reflection test cases") {
    val a = Vector3(10, 0, 0)
    val b = Vector3(0, 10, 0)
    val c = Vector3(-10, 0, 0)
    
    //Should not intersect
    assert(UNIT_SPHERE.reflect(a, c) === null)
    val r = UNIT_SPHERE.reflect(a, b)
    //Should intersect
    assert(r != null)
    val expected = Vector3(1, 1, 0).normalized
    assert(abs((r - expected).mag) < 0.00001)
  }

  test("reflect") {
    val a = Vector3(1.1, 0, 0)
    val b = Vector3(1.5, 0.01, 0)
    val r = UNIT_SPHERE.reflect(a, b)
    assert(r != null, "r was null.")
  }
  
  test("Ray intersection test") {
    val a = Vector3(1.1, 0, 0)
    val b = Vector3(1.5, 0.01, 0)
    val ray = Ray.fromPoints(b, a)
    assert((ray --> UNIT_SPHERE).length == 2, "Ray size was not 2.")
  }

  test("sphere-plane - normals aligned") {
    val p = Plane(1.0, Vector3(0, 1, 1))
    val s = Sphere(Vector3(1, 1, 1), 2.0)
    val intersection = s.intersect(p)
    assert(p.n * (intersection.xAxis % intersection.yAxis) >= 0)
  }

  test("sphere-plane - flipped normals aligned") {
    val p = Plane(-1.0, Vector3(0, -1, -1))
    val s = Sphere(Vector3(1, 1, 1), 2.0)
    val intersection = s.intersect(p)
    assert(p.n * (intersection.xAxis % intersection.yAxis) >= 0)
  }

  test("sphere-plane - tangent plane") {
    val p = Plane(3.0, Vector3(0, 1, 0))
    val s = Sphere(Vector3(1, 1, 1), 2.0)
    val intersection = s.intersect(p)
    assert(intersection.radius == 0)
  }

  test("sphere-plane - no intersection") {
    val p = Plane(3.01, Vector3(0, 1, 0))
    val s = Sphere(Vector3(1, 1, 1), 2.0)
    val intersection = s.intersect(p)
    assert(intersection == null)
  }

  test("sphere-plane - flipped no intersection") {
    val p = Plane(-3.01, Vector3(0, -1, 0))
    val s = Sphere(Vector3(1, 1, 1), 2.0)
    val intersection = s.intersect(p)
    assert(intersection == null)
  }

  test("sphere-plane - flipped intersection normal same as plane normal") {
    val p = Plane(-1.0, Vector3(0, -1, -1))
    val s = Sphere(Vector3(1, 1, 1), 2.0)
    val intersection = s.intersect(p)
    val r = intersection.plane
    assert(p.n close r.n)
  }

  test("sphere-plane - intersection normal same as plane norma") {
    val p = Plane(1.0, Vector3(0, 1, 1))
    val s = Sphere(Vector3(1, 1, 1), 2.0)
    val intersection = s.intersect(p)
    val r = intersection.plane
    assert(p.n close r.n)
  }
  
  test("before-before") {
    val ls = LineSegment(Vector3(-3, 0, 0), Vector3(-2, 0, 0))
    assert(!(UNIT_SPHERE intersects ls))
    val res = UNIT_SPHERE intersect ls
    assert(res.length == 0, "Should have had 0 intersections instead of " + res.length + ".")
  }
  
  test("before-on") {
    val ls = LineSegment(Vector3(-3, 0, 0), Vector3(-1, 0, 0))
    assert(UNIT_SPHERE intersects ls)
    val res = UNIT_SPHERE intersect ls
    assert(res.length == 1, "Should have had 0 intersections instead of " + res.length + ".")
    assert(res(0) == 1.0)
  }
  
  test("before-in") {
    val ls = LineSegment(Vector3(-2, 0, 0), Vector3(0, 0, 0))
    assert(UNIT_SPHERE intersects ls)
    val res = UNIT_SPHERE intersect ls
    assert(res.length == 1, "Should have had 1 intersection instead of " + res.length + ".")
    assert(res(0) == 0.5, "First position was " + res(0) + " instead of 0.5.")
  }
  
  test("on-in") {
    val ls = LineSegment(Vector3(-1, 0, 0), Vector3(0, 0, 0))
    assert(UNIT_SPHERE intersects ls)
    val res = UNIT_SPHERE intersect ls
    assert(res.length == 1, "Should have had 1 intersection instead of " + res.length + ".")
    assert(res(0) == 0.0, "First position was " + res(0) + " instead of 0.0.")
  }
  
  test("in-in") {
    val ls = LineSegment(Vector3(-0.5, 0, 0), Vector3(0.5, 0, 0))
    assert(!(UNIT_SPHERE intersects ls))
    val res = UNIT_SPHERE intersect ls
    assert(res.length == 0, "Should have had 0 intersections instead of " + res.length + ".")
  }
  
  test("on-on") {
    val ls = LineSegment(Vector3(-1, 0, 0), Vector3(1, 0, 0))
    assert(UNIT_SPHERE intersects ls)
    val res = UNIT_SPHERE intersect ls
    assert(res.length == 2, "Should have had 2 intersections instead of " + res.length + ".")
    assert(res(0) == 0.0, "First position was " + res(0) + " instead of 0.0.")
    assert(res(1) == 1.0, "Second position was " + res(1) + " instead of 1.0.")
  }
  
  test("in-on") {
    val ls = LineSegment(Vector3(0, 0, 0), Vector3(1, 0, 0))
    assert(UNIT_SPHERE intersects ls)
    val res = UNIT_SPHERE intersect ls
    assert(res.length == 1, "Should have had 1 intersection instead of " + res.length + ".")
    assert(res(0) == 1.0, "First position was " + res(0) + " instead of 1.0.")
  }
  
  test("in-after") {
    val ls = LineSegment(Vector3(0, 0, 0), Vector3(2, 0, 0))
    assert(UNIT_SPHERE intersects ls)
    val res = UNIT_SPHERE intersect ls
    assert(res.length == 1, "Should have had 1 intersection instead of " + res.length + ".")
    assert(res(0) == 0.5, "First position was " + res(0) + " instead of 0.5.")
  }
  
  test("on-after") {
    val ls = LineSegment(Vector3(1, 0, 0), Vector3(2, 0, 0))
    assert(UNIT_SPHERE intersects ls)
    val res = UNIT_SPHERE intersect ls
    assert(res.length == 1, "Should have had 1 intersection instead of " + res.length + ".")
    assert(res(0) == 0.0, "First position was " + res(0) + " instead of 0.0.")
  }
  
  test("after-after") {
    val ls = LineSegment(Vector3(2, 0, 0), Vector3(3, 0, 0))
    assert(!(UNIT_SPHERE intersects ls))
    val res = UNIT_SPHERE intersect ls
    assert(res.length == 0, "Should have had no intersections instead of " + res.length + ".")
  }

  test("before-after") {
    val ls = LineSegment(Vector3(-2, 0, 0), Vector3(2, 0, 0))
    assert(UNIT_SPHERE intersects ls)
    val res = UNIT_SPHERE intersect ls
    assert(res.length == 2, "Should have had 2 intersections instead of " + res.length + ".")
    assert(res(0) == 0.25, "First position was " + res(0) + " instead of 0.25.")
    assert(res(1) == 0.75, "Second position was " + res(1) + " instead of 0.75.")
  }

  test("tangent circle") {
    val p = X_AXIS * 2

    (UNIT_SPHERE tangentRange p) foreach { range => assert(range === sqrt(3.0), "Range") }
    (UNIT_SPHERE tangentAngle p) foreach { theta => assert(abs(theta.toDegrees - 30.0) <= 1.0E-10, "Tangent Angle") }
    (UNIT_SPHERE tangentCircle p) foreach { c => assert(c.radius === sqrt(3.0) * 0.5, "Radius") }
  }
}