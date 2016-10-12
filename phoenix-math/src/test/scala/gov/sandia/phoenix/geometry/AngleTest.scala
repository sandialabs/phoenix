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

class AngleTest extends FunSuite {

  test("delta") {
    assert(Degrees(90) - Degrees(88) == Degrees(2))
    assert(Degrees(88) - Degrees(90) == Degrees(-2))
    assert(Degrees(270) - Degrees(2) == Degrees(268))
    assert(Degrees(2) - Degrees(270) == Degrees(-268))
  }

  test("constrain signed") {
    assert((Degrees(90) - Degrees(88)).constrainSigned == Degrees(2))
    assert((Degrees(88) - Degrees(90)).constrainSigned == Degrees(-2))
    assert((Degrees(270) - Degrees(2)).constrainSigned == Degrees(-92))
    assert((Degrees(2) - Degrees(270)).constrainSigned == Degrees(92))
  }

  test("constrain unsigned") {
    assert((Degrees(90) - Degrees(88)).constrainUnsigned == Degrees(2))
    assert((Degrees(88) - Degrees(90)).constrainUnsigned == Degrees(358))
    assert((Degrees(270) - Degrees(2)).constrainUnsigned == Degrees(268))
    assert((Degrees(2) - Degrees(270)).constrainUnsigned == Degrees(92))
  }
}
