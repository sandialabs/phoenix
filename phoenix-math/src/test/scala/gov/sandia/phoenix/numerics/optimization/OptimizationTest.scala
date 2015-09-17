package gov.sandia.phoenix.numerics

import org.scalatest.FunSuite

class OptimizationTest extends FunSuite
{
  test("goldenMax") {
    def f(t : Double) = - t * t - 5.0
    val max = optimization.goldenMax(-5.0, 5.0, 1E-3)(f)
    assert(max._2 == -5.0)
  }
      
  test("goldenMin") {
    def f(t : Double) = t * t - 5.0
    val max = optimization.goldenMin(-5.0, 5.0, 1E-3)(f)
    assert(max._2 == -5.0)
  }
}