package gov.sandia.phoenix.geometry

import org.scalatest.FunSuite

class RayTest extends FunSuite
{
  test("fromPoints")
  {
    val p0 = new Vector3(2, 3, 5)
    val p1 = new Vector3(2, 3, 8)
    val ray = Ray.fromPoints(p0, p1)
        
    assert(ray.direction.mag == 1.0)
    val v = new Vector3(0, 0, 1)
    assert(ray.direction == v)
  }
}