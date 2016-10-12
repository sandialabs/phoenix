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

class LineSegmentTest extends FunSuite
{
  test("pass through")
  {
    val p0 = new Vector3(-5, 0, 0)
    val p1 = new Vector3(5, 0, 0)
    val ls = new LineSegment(p0, p1)
    val s = new Sphere()
    val res = s.intersect(ls)
    assert(res.length == 2)
  }
      
  test("graze")
  {
    val p0 = new Vector3(-5, 1, 0)
    val p1 = new Vector3(5, 1, 0)
    val ls = new LineSegment(p0, p1)
    val s = new Sphere()
    val res = s.intersect(ls)
    assert(res.length == 1)
  }
      
  test("P0 out P1 in")
  {
    val p0 = new Vector3(-5, 0, 0)
    val p1 = new Vector3(0, 0, 0)
    val ls = new LineSegment(p0, p1)
    val s = new Sphere()
    val res = s.intersect(ls)
    assert(res.length == 1)
  }
      
  test("P0 in P1 out")
  {
    val p0 = new Vector3(0, 0, 0)
    val p1 = new Vector3(5, 0, 0)
    val ls = new LineSegment(p0, p1)
    val s = new Sphere()
    val res = s.intersect(ls)
    assert(res.length == 1)
  }
      
  test("miss")
  {
    val p0 = new Vector3(-5, 2, 0)
    val p1 = new Vector3(5, 2, 0)
    val ls = new LineSegment(p0, p1)
    val s = new Sphere()
    val res = s.intersect(ls)
    assert(res.length == 0)
  }
      
  test("ends on")
  {
    val p0 = new Vector3(-1, 0, 0)
    val p1 = new Vector3(1, 0, 0)
    val ls = new LineSegment(p0, p1)
    val s = new Sphere()
    val res = s.intersect(ls)
    assert(res.length == 2)
  }
      
  test("P0 on")
  {
    val p0 = new Vector3(1, 0, 0)
    val p1 = new Vector3(2, 0, 0)
    val ls = new LineSegment(p0, p1)
    val s = new Sphere()
    val res = s.intersect(ls)
    assert(res.length == 1)
  }
      
  test("P1 on")
  {
    val p0 = new Vector3(-2, 0, 0)
    val p1 = new Vector3(-1, 0, 0)
    val ls = new LineSegment(p0, p1)
    val s = new Sphere()
    val res = s.intersect(ls)
    assert(res.length == 1)
  }
      
  test("completely before")
  {
    val p0 = new Vector3(-2, 0, 0)
    val p1 = new Vector3(-1.001, 0, 0)
    val ls = new LineSegment(p0, p1)
    val s = new Sphere()
    val res = s.intersect(ls)
    assert(res.length == 0)
  }
      
  test("completely after")
  {
    val p0 = new Vector3(1.001, 0, 0)
    val p1 = new Vector3(2, 0, 0)
    val ls = new LineSegment(p0, p1)
    val s = new Sphere()
    val res = s.intersect(ls)
    assert(res.length == 0)
  }

  test("perpendicular distance"){
    val a = new LineSegment(Vector3(-1, 0, 0), Vector3(1, 0, 0))
    val b = new LineSegment(Vector3(0, -1, 1), Vector3(0, 1, 1))
    a.closestPoint(b) match {
      case Some((u, v)) =>
        assert(u === 0.5)
        assert(v === 0.5)
      case None => assert(false)
    }
  }

  test("parallel distance"){
    val a = new LineSegment(Vector3(-1, 0, 0), Vector3(1, 0, 0))
    val b = new LineSegment(Vector3(-1, 0, 1), Vector3(1, 0, 1))
    a.closestPoint(b) match {
      case Some((u, v)) => assert(false)
      case None => assert(true)
    }
  }
}