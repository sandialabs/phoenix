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
