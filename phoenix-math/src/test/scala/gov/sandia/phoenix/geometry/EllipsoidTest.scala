package gov.sandia.phoenix.geometry



import org.scalatest.FunSuite


class EllipsoidTest extends FunSuite
{
  test("before-before") {
    val s = new Ellipsoid(ORIGIN, new Vector3(1.0, 1.0, 5.0))
    val ls = new LineSegment(new Vector3(-3, 0, 0), new Vector3(-2, 0, 0))
    assert(!(s intersects ls))
    val res = s intersect ls
    assert(res.length == 0, "Should have had 0 intersections instead of " + res.length + ".")
  }
  
  test("before-on") {
    val s = new Ellipsoid(ORIGIN, new Vector3(1.0, 1.0, 5.0))
    val ls = new LineSegment(new Vector3(-3, 0, 0), new Vector3(-1, 0, 0))
    assert(s intersects ls)
    val res = s intersect ls
    assert(res.length == 1, "Should have had 0 intersections instead of " + res.length + ".")
    assert(res(0) == 1.0)
  }
  
  test("before-in") {
    val s = new Ellipsoid(ORIGIN, new Vector3(1.0, 1.0, 5.0))
    val ls = new LineSegment(new Vector3(-2, 0, 0), new Vector3(0, 0, 0))
    assert(s intersects ls)
    val res = s intersect ls
    assert(res.length == 2, "Should have had 2 intersections instead of " + res.length + ".")
    assert(res(0) == 0.5, "First position was " + res(0) + " instead of 0.5.")
    assert(res(1) == 1.0, "Second position was " + res(1) + " instead of 1.0.")
  }
  
  test("on-in") {
    val s = new Ellipsoid(ORIGIN, new Vector3(1.0, 1.0, 5.0))
    val ls = new LineSegment(new Vector3(-1, 0, 0), new Vector3(0, 0, 0))
    assert(s intersects ls)
    val res = s intersect ls
    assert(res.length == 2, "Should have had 2 intersections instead of " + res.length + ".")
    assert(res(0) == 0.0, "First position was " + res(0) + " instead of 0.0.")
    assert(res(1) == 1.0, "Second position was " + res(1) + " instead of 1.0.")
  }
  
  test("in-in") {
    val s = new Ellipsoid(ORIGIN, new Vector3(1.0, 1.0, 5.0))
    val ls = new LineSegment(new Vector3(-0.5, 0, 0), new Vector3(0.5, 0, 0))
    assert(s intersects ls)
    val res = s intersect ls
    assert(res.length == 2, "Should have had 2 intersections instead of " + res.length + ".")
    assert(res(0) == 0.0, "First position was " + res(0) + " instead of 0.0.")
    assert(res(1) == 1.0, "Second position was " + res(1) + " instead of 1.0.")
  }
  
  test("on-on") {
    val s = new Ellipsoid(ORIGIN, new Vector3(1.0, 1.0, 5.0))
    val ls = new LineSegment(new Vector3(-1, 0, 0), new Vector3(1, 0, 0))
    assert(s intersects ls)
    val res = s intersect ls
    assert(res.length == 2, "Should have had 2 intersections instead of " + res.length + ".")
    assert(res(0) == 0.0, "First position was " + res(0) + " instead of 0.0.")
    assert(res(1) == 1.0, "Second position was " + res(1) + " instead of 1.0.")
  }
  
  test("in-on") {
    val s = new Ellipsoid(ORIGIN, new Vector3(1.0, 1.0, 5.0))
    val ls = new LineSegment(new Vector3(0, 0, 0), new Vector3(1, 0, 0))
    assert(s intersects ls)
    val res = s intersect ls
    assert(res.length == 2, "Should have had 2 intersections instead of " + res.length + ".")
    assert(res(0) == 0.0, "First position was " + res(0) + " instead of 0.0.")
    assert(res(1) == 1.0, "Second position was " + res(1) + " instead of 1.0.")
  }
  
  test("in-after") {
    val s = new Ellipsoid(ORIGIN, new Vector3(1.0, 1.0, 5.0))
    val ls = new LineSegment(new Vector3(0, 0, 0), new Vector3(2, 0, 0))
    assert(s intersects ls)
    val res = s intersect ls
    assert(res.length == 2, "Should have had 2 intersections instead of " + res.length + ".")
    assert(res(0) == 0.0, "First position was " + res(0) + " instead of 0.0.")
    assert(res(1) == 0.5, "Second position was " + res(1) + " instead of 0.5.")
  }
  
  test("on-after") {
    val s = new Ellipsoid(ORIGIN, new Vector3(1.0, 1.0, 5.0))
    val ls = new LineSegment(new Vector3(1, 0, 0), new Vector3(2, 0, 0))
    assert(s intersects ls)
    val res = s intersect ls
    assert(res.length == 1, "Should have had 1 intersection instead of " + res.length + ".")
    assert(res(0) == 0.0, "First position was " + res(0) + " instead of 0.0.")
  }
  
  test("after-after") {
    val s = new Ellipsoid(ORIGIN, new Vector3(1.0, 1.0, 5.0))
    val ls = new LineSegment(new Vector3(2, 0, 0), new Vector3(3, 0, 0))
    assert(!(s intersects ls))
    val res = s intersect ls
    assert(res.length == 0, "Should have had no intersections instead of " + res.length + ".")
  }
  
  test("before-after") {
    val s = new Ellipsoid(ORIGIN, new Vector3(1.0, 1.0, 5.0))
    val ls = new LineSegment(new Vector3(-2, 0, 0), new Vector3(2, 0, 0))
    assert(s intersects ls)
    val res = s intersect ls
    assert(res.length == 2, "Should have had 2 intersections instead of " + res.length + ".")
    assert(res(0) == 0.25, "First position was " + res(0) + " instead of 0.25.")
    assert(res(1) == 0.75, "Second position was " + res(1) + " instead of 0.75.")
  }

  test("ray-miss") {
    val s = new Ellipsoid(ORIGIN, new Vector3(1.0, 1.0, 5.0))
    val r = new Ray(new Vector3(2, 0, 0), new Vector3(0, 0, 1))
    assert(!(s intersects r))
    val res = s intersect r
    assert(res.length == 0, "Should have had no intersections instead of " + res.length + ".")
  }
}