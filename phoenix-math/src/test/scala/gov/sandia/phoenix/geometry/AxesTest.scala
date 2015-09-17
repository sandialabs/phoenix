package gov.sandia.phoenix.geometry

import org.scalatest.FunSuite
import scala.math._

class AxesTest extends FunSuite {
  test("XY") {
    assert(IDENTITY_QUATERNION == Axes.XY(X_AXIS * random, Y_AXIS * random))
  }

  test("XZ") {
    assert(IDENTITY_QUATERNION == Axes.XZ(X_AXIS * random, Z_AXIS * random))
  }

  test("YX") {
    assert(IDENTITY_QUATERNION == Axes.YX(Y_AXIS * random, X_AXIS * random))
  }

  test("YZ") {
    assert(IDENTITY_QUATERNION == Axes.YZ(Y_AXIS * random, Z_AXIS * random))
  }

  test("ZX") {
    assert(IDENTITY_QUATERNION == Axes.ZX(Z_AXIS * random, X_AXIS * random))
  }

  test("ZY") {
    assert(IDENTITY_QUATERNION == Axes.ZY(Z_AXIS * random, Y_AXIS * random))
  }
}
