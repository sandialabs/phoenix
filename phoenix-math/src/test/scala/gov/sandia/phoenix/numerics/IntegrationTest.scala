package gov.sandia.phoenix.numerics

import org.scalatest.FunSuite
import scala.math._

class IntegrationTest extends FunSuite {
  test("sin(x) 0 -> Pi"){
    assert(abs(2.0 - integration.romberg(0, Pi, 10, 1.0E-10){ x => sin(x) }) <= 1.0E-10)
  }

  test("sin(x) 0 -> 2.0 * Pi"){
    assert(abs(integration.romberg(0, 2.0 * Pi, 10, 1.0E-10){ x => sin(x) }) <= 1.0E-10)
  }

  test("length of circle should equal diameter"){
    val l = integration.romberg(0, 2.0 * Pi, 10, 1.0E-10){ x =>
      val dx = -sin(x)
      val dy = cos(x)
      sqrt(dx * dx + dy * dy)
    }
    assert(abs(2.0 * Pi - l) <= 1.0E-10)
  }
}
