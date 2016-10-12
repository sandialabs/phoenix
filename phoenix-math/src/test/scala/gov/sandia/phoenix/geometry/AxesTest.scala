/*
 * Copyright (c) 2016 Sandia Corporation. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by the
 * terms of this license.
 * You must not remove this notice, or any other, from this software.
 *
 * Contributors:
 * - Mark Bastian: Original author.
 * - See Git logs.
 */

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
