package gov.sandia.phoenix.geometry



import org.scalatest.FunSuite


class PlaneTest extends FunSuite
{
  test("dist - x") {
    val plane = new Plane(2.0, new Vector3(1, 0, 0))
    assert(plane.dist(new Vector3(5, 5, 3)) == 3)
    assert(plane.dist(new Vector3(-5, 6, 1)) == -7)
  }

  test("dist - y") {
    val plane = new Plane(2.0, new Vector3(0, 1, 0))
    assert(plane.dist(new Vector3(5, 5, 3)) == 3)
    assert(plane.dist(new Vector3(6, -5, 1)) == -7)
  }

  test("dist - z") {
    val plane = new Plane(2.0, new Vector3(0, 0, 1))
    assert(plane.dist(new Vector3(5, 3, 5)) == 3)
    assert(plane.dist(new Vector3(6, 1, -5)) == -7)

    val flipped = plane.flip
    assert(flipped.dist(new Vector3(5, 3, 5)) == -3)
    assert(flipped.dist(new Vector3(6, 1, -5)) == 7)
  }
      
  test("above") {
    val plane = new Plane(2.0, new Vector3(1, 1, 1))
    val p = new Vector3(0, 0, 0)
    assert(plane > p)
    assert(p < plane)
  }
      
  test("below") {
    val plane = new Plane(2.0, new Vector3(1, 1, 1))
    val p = new Vector3(5, 5, 5)
    assert(plane < p)
    assert(p > plane)
  }
      
  test("on") {
    val plane = new Plane(2.0, new Vector3(1, 1, 1))
    val p = new Vector3(1, 1, 1).normalized
    val px2 = p * 2
    assert(plane == px2)
    assert(px2 == plane)
  }
      
  test("shift") {
    var plane = new Plane(2.0, new Vector3(0, 1, 0))
    plane = plane + 2.0
    assert(plane.d == 4.0)
    var p = new Vector3(5, 4, 3)
    assert(plane == p)
  }

  test("intersect parallel planes"){
    assert((X_PLANE - 2).intersect(X_PLANE + 2).isEmpty)
    assert((Y_PLANE - 2).intersect(Y_PLANE + 2).isEmpty)
    assert((Z_PLANE - 2).intersect(Z_PLANE + 2).isEmpty)
  }

  test("intersect same planes"){
    assert(X_PLANE.intersect(X_PLANE).isEmpty)
    assert(Y_PLANE.intersect(Y_PLANE).isEmpty)
    assert(Z_PLANE.intersect(Z_PLANE).isEmpty)
  }

  test("intersect orthogonal planes"){
    assert(X_PLANE.intersect(Y_PLANE).contains(new Ray(ORIGIN, Z_AXIS)))
    assert(X_PLANE.intersect(Z_PLANE).contains(new Ray(ORIGIN, -Y_AXIS)))
    assert(Y_PLANE.intersect(X_PLANE).contains(new Ray(ORIGIN, -Z_AXIS)))
    assert(Y_PLANE.intersect(Z_PLANE).contains(new Ray(ORIGIN, X_AXIS)))
    assert(Z_PLANE.intersect(X_PLANE).contains(new Ray(ORIGIN, Y_AXIS)))
    assert(Z_PLANE.intersect(Y_PLANE).contains(new Ray(ORIGIN, -X_AXIS)))
  }

  test("intersect shifted orthogonal planes"){
    assert(X_PLANE.intersect(Y_PLANE + 1).contains(new Ray(Vector3(0, 1, 0), Z_AXIS)))
    assert(X_PLANE.intersect(Z_PLANE + 1).contains(new Ray(Vector3(0, 0, 1), -Y_AXIS)))
    assert(Y_PLANE.intersect(X_PLANE + 1).contains(new Ray(Vector3(1, 0, 0), -Z_AXIS)))
    assert(Y_PLANE.intersect(Z_PLANE + 1).contains(new Ray(Vector3(0, 0, 1), X_AXIS)))
    assert(Z_PLANE.intersect(X_PLANE + 1).contains(new Ray(Vector3(1, 0, 0), Y_AXIS)))
    assert(Z_PLANE.intersect(Y_PLANE + 1).contains(new Ray(Vector3(0, 1, 0), -X_AXIS)))
  }
}