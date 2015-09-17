package gov.sandia.phoenix.geometry

import org.scalatest.FunSuite

class SphericalPolygonTest extends FunSuite {
  test("antipode in"){
    val out = Z_AXIS
    val pts = Array(X_AXIS, Y_AXIS, -X_AXIS, -Y_AXIS)

    val sp = SphericalPolygon(pts, out)

    val a = Arc(out, X_AXIS)
    val b = Arc(X_AXIS, -out)
    assert((sp crossings a) == 0)
    assert((sp crossings b) == 1)

    assert(!sp.contains(out))
    assert(sp.contains(-out))
  }
}
