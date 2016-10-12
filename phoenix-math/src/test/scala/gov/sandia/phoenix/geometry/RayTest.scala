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