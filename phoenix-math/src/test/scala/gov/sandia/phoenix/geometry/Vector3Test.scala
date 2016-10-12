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

class Vector3Test extends FunSuite
{
  test("dot")
  {
    assert(X_AXIS * Y_AXIS == 0.0)
    assert(Y_AXIS * Z_AXIS == 0.0)
    assert(X_AXIS * Z_AXIS == 0.0)

    val u = Vector3(3, 2, 6)
    val v = Vector3(2, 1, 5)
    val expected = 3 * 2 + 2 * 1 + 6 * 5
    assert(u * v == expected)
    assert(u.dot(v) == expected)
  }
  test("cross") {  assert((X_AXIS % Y_AXIS) == Z_AXIS) }
  
  test("perp") { assert(~X_AXIS*X_AXIS == 0) }
  
  test("negate")
  {
    val u = new Vector3(1, 1, 1)
    val x = -u.x
    val y = -u.y
    val z = -u.z
    val nu = u.negated
    assert(x == nu.x)
    assert(y == nu.z)
    assert(z == nu.z)
  }
      
  test("sweep"){
    val res = cos(toRadians(45.0))
    for(vec <-Y_AXIS.sweep(toRadians(45.0), 100)) 
      assert(abs(vec * Y_AXIS - res) < 1E-10)
  }

}