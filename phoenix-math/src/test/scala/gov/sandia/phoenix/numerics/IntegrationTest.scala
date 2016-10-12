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
