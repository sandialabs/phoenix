package gov.sandia.phoenix.numerics

import org.scalatest.FunSuite

import scala.math._

class NumericsTest extends FunSuite {
  test("bisect")
  {
    def f(t : Double) = t * t - 5.0
    val root = Numerics.bisect(f, -1, 10)
    assert(abs(f(root)) < 1E-10)
  }
      
  test("falsepos")
  {
    def f(t : Double) = t * t - 5.0
    val root = Numerics.falsepos(f, -1, 10)
    assert(abs(f(root)) < 1E-10)
  }
}