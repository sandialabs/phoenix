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

import gov.sandia.phoenix.constants.WGS84
import org.scalatest.FunSuite

class WGS84EllipsoidTest extends FunSuite
{
  test("intersectNadir")
  {
    val ecef = new Vector3(WGS84.R_EQ_M * 2, WGS84.R_EQ_M * 2, WGS84.R_EQ_M * 2)
    val dir = -ecef.normalized
    val ray = new Ray(ecef, dir)
    val intersects = WGS84.ellipsoid.intersect(ray)
    assert(intersects.size === 2)
  }

  test("intersectExample") {
    val p = new Vector3(WGS84.R_EQ_M * 2, 0, 0)
    val ray = new Ray(p, p.normalized.negated)
    val intersects = WGS84.ellipsoid.closestIntersection(ray)
    val ans = new Vector3(WGS84.R_EQ_M, 0, 0)
    assert(ans close intersects)
  }
}