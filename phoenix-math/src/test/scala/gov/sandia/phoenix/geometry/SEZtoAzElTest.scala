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

class SEZtoAzElTest extends FunSuite {
  val S = Vector3(1, 0, 0)
  val E = Vector3(0, 1, 0)
  val Z = Vector3(0, 0, 1)
  val N = Vector3(-1, 0, 0)
  val W = Vector3(0, -1, 0)
  val D = Vector3(0, 0, -1)

  test("S") {
    val azelr = S.toAzElR
    assert(azelr.azimuth === 180.0)
    assert(azelr.elevation === 0.0)

    val azel = S.toAzEl
    assert(azel.azimuth === 180.0)
    assert(azel.elevation === 0.0)
  }

  test("E") {
    val azelr = E.toAzElR
    assert(azelr.azimuth === 90.0)
    assert(azelr.elevation === 0.0)

    val azel = E.toAzElR
    assert(azel.azimuth === 90.0)
    assert(azel.elevation === 0.0)
  }

  test("Z") {
    val azelr = Z.toAzElR
    assert(azelr.azimuth === 180.0)
    assert(azelr.elevation === 90.0)

    val azel = Z.toAzElR
    assert(azel.azimuth === 180.0)
    assert(azel.elevation === 90.0)
  }

  test("N") {
    val azelr = N.toAzElR
    assert(azelr.azimuth === 0.0)
    assert(azelr.elevation === 0.0)

    val azel = N.toAzElR
    assert(azel.azimuth === 0.0)
    assert(azel.elevation === 0.0)
  }

  test("W") {
    val azelr = W.toAzElR
    assert(azelr.azimuth === 270.0)
    assert(azelr.elevation === 0.0)

    val azel = W.toAzElR
    assert(azel.azimuth === 270.0)
    assert(azel.elevation === 0.0)
  }

  test("D") {
    val azelr = D.toAzElR
    assert(azelr.azimuth === 180.0)
    assert(azelr.elevation === -90.0)

    val azel = D.toAzElR
    assert(azel.azimuth === 180.0)
    assert(azel.elevation === -90.0)
  }
}
