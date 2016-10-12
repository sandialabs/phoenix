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

class EllipsoidShellTest extends FunSuite {
 test("before-before") {
    val s = new EllipsoidShell(ORIGIN, new Vector3(1.0, 1.0, 5.0))
    val ls = new LineSegment(new Vector3(-3, 0, 0), new Vector3(-2, 0, 0))
    assert(!(s intersects ls))
    val res = s intersect ls
    assert(res.length == 0, "Should have had 0 intersections instead of " + res.length + ".")
  }
  
  test("before-on") {
    val s = new EllipsoidShell(ORIGIN, new Vector3(1.0, 1.0, 5.0))
    val ls = new LineSegment(new Vector3(-3, 0, 0), new Vector3(-1, 0, 0))
    assert(s intersects ls)
    val res = s intersect ls
    assert(res.length == 1, "Should have had 0 intersections instead of " + res.length + ".")
    assert(res(0) == 1.0)
  }
  
  test("before-in") {
    val s = new EllipsoidShell(ORIGIN, new Vector3(1.0, 1.0, 5.0))
    val ls = new LineSegment(new Vector3(-2, 0, 0), new Vector3(0, 0, 0))
    assert(s intersects ls)
    val res = s intersect ls
    assert(res.length == 1, "Should have had 1 intersection instead of " + res.length + ".")
    assert(res(0) == 0.5, "First position was " + res(0) + " instead of 0.5.")
  }
  
  test("on-in") {
    val s = new EllipsoidShell(ORIGIN, new Vector3(1.0, 1.0, 5.0))
    val ls = new LineSegment(new Vector3(-1, 0, 0), new Vector3(0, 0, 0))
    assert(s intersects ls)
    val res = s intersect ls
    assert(res.length == 1, "Should have had 1 intersection instead of " + res.length + ".")
    assert(res(0) == 0.0, "First position was " + res(0) + " instead of 0.0.")
  }
  
  test("in-in") {
    val s = new EllipsoidShell(ORIGIN, new Vector3(1.0, 1.0, 5.0))
    val ls = new LineSegment(new Vector3(-0.5, 0, 0), new Vector3(0.5, 0, 0))
    assert(!(s intersects ls))
    val res = s intersect ls
    assert(res.length == 0, "Should have had 0 intersections instead of " + res.length + ".")
  }
  
  test("on-on") {
    val s = new EllipsoidShell(ORIGIN, new Vector3(1.0, 1.0, 5.0))
    val ls = new LineSegment(new Vector3(-1, 0, 0), new Vector3(1, 0, 0))
    assert(s intersects ls)
    val res = s intersect ls
    assert(res.length == 2, "Should have had 2 intersections instead of " + res.length + ".")
    assert(res(0) == 0.0, "First position was " + res(0) + " instead of 0.0.")
    assert(res(1) == 1.0, "Second position was " + res(1) + " instead of 1.0.")
  }
  
  test("in-on") {
    val s = new EllipsoidShell(ORIGIN, new Vector3(1.0, 1.0, 5.0))
    val ls = new LineSegment(new Vector3(0, 0, 0), new Vector3(1, 0, 0))
    assert(s intersects ls)
    val res = s intersect ls
    assert(res.length == 1, "Should have had 1 intersection instead of " + res.length + ".")
    assert(res(0) == 1.0, "First position was " + res(0) + " instead of 1.0.")
  }
  
  test("in-after") {
    val s = new EllipsoidShell(ORIGIN, new Vector3(1.0, 1.0, 5.0))
    val ls = new LineSegment(new Vector3(0, 0, 0), new Vector3(2, 0, 0))
    assert(s intersects ls)
    val res = s intersect ls
    assert(res.length == 1, "Should have had 1 intersection instead of " + res.length + ".")
    assert(res(0) == 0.5, "First position was " + res(0) + " instead of 0.5.")
  }
  
  test("on-after") {
    val s = new EllipsoidShell(ORIGIN, new Vector3(1.0, 1.0, 5.0))
    val ls = new LineSegment(new Vector3(1, 0, 0), new Vector3(2, 0, 0))
    assert(s intersects ls)
    val res = s intersect ls
    assert(res.length == 1, "Should have had 1 intersection instead of " + res.length + ".")
    assert(res(0) == 0.0, "First position was " + res(0) + " instead of 0.0.")
  }
  
  test("after-after") {
    val s = new EllipsoidShell(ORIGIN, new Vector3(1.0, 1.0, 5.0))
    val ls = new LineSegment(new Vector3(2, 0, 0), new Vector3(3, 0, 0))
    assert(!(s intersects ls))
    val res = s intersect ls
    assert(res.length == 0, "Should have had no intersections instead of " + res.length + ".")
  }
  
  test("before-after") {
    val s = new EllipsoidShell(ORIGIN, new Vector3(1.0, 1.0, 5.0))
    val ls = new LineSegment(new Vector3(-2, 0, 0), new Vector3(2, 0, 0))
    assert(s intersects ls)
    val res = s intersect ls
    assert(res.length == 2, "Should have had 2 intersections instead of " + res.length + ".")
    assert(res(0) == 0.25, "First position was " + res(0) + " instead of 0.25.")
    assert(res(1) == 0.75, "Second position was " + res(1) + " instead of 0.75.")
  }
}
