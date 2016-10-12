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

class AzEltoSEZTest extends FunSuite {
  test("Z") {
    val azelr = new AzElR(0, 90, 1.0)
    val sez = azelr.toSEZ
    assert(abs(sez.x) <= 1.0E-12)
    assert(abs(sez.y) <= 1.0E-12)
    assert(abs(sez.z - 1.0) <= 1.0E-12)

    val sez1 = new AzEl(0, 90).toSEZ
    assert(abs(sez1.x) <= 1.0E-12)
    assert(abs(sez1.y) <= 1.0E-12)
    assert(abs(sez1.z - 1.0) <= 1.0E-12)
  }

  test("-Z") {
    val azelr = new AzElR(0, -90, 1.0)
    val sez = azelr.toSEZ
    assert(abs(sez.x) <= 1.0E-12)
    assert(abs(sez.y) <= 1.0E-12)
    assert(abs(sez.z + 1.0) <= 1.0E-12)

    val sez1 = new AzEl(0, -90).toSEZ
    assert(abs(sez1.x) <= 1.0E-12)
    assert(abs(sez1.y) <= 1.0E-12)
    assert(abs(sez1.z + 1.0) <= 1.0E-12)
  }

  test("N") {
    val azelr = new AzElR(0, 0, 1.0)
    val sez = azelr.toSEZ
    assert(abs(1.0 + sez.x) <= 1.0E-12)
    assert(abs(sez.y) <= 1.0E-12)
    assert(abs(sez.z) <= 1.0E-12)

    val sez1 = new AzEl(0, 0).toSEZ
    assert(abs(1.0 + sez1.x) <= 1.0E-12)
    assert(abs(sez1.y) <= 1.0E-12)
    assert(abs(sez1.z) <= 1.0E-12)
  }

  test("S") {
    val azelr = new AzElR(180, 0, 1.0)
    val sez = azelr.toSEZ
    assert(abs(-1.0 + sez.x) <= 1.0E-12)
    assert(abs(sez.y) <= 1.0E-12)
    assert(abs(sez.z) <= 1.0E-12)

    val sez1 = new AzEl(180, 0).toSEZ
    assert(abs(-1.0 + sez1.x) <= 1.0E-12)
    assert(abs(sez1.y) <= 1.0E-12)
    assert(abs(sez1.z) <= 1.0E-12)
  }

  test("E") {
    val azelr = new AzElR(90, 0, 1.0)
    val sez = azelr.toSEZ
    assert(abs(sez.x) <= 1.0E-12)
    assert(abs(1.0 - sez.y) <= 1.0E-12)
    assert(abs(sez.z) <= 1.0E-12)

    val sez1 = new AzEl(90, 0).toSEZ
    assert(abs(sez1.x) <= 1.0E-12)
    assert(abs(1.0 - sez1.y) <= 1.0E-12)
    assert(abs(sez1.z) <= 1.0E-12)
  }

  test("W") {
    val azelr = new AzElR(270, 0, 1.0)
    val sez = azelr.toSEZ
    assert(abs(sez.x) <= 1.0E-12)
    assert(abs(1.0 + sez.y) <= 1.0E-12)
    assert(abs(sez.z) <= 1.0E-12)

    val sez1 = new AzEl(270, 0).toSEZ
    assert(abs(sez1.x) <= 1.0E-12)
    assert(abs(1.0 + sez1.y) <= 1.0E-12)
    assert(abs(sez1.z) <= 1.0E-12)
  }
}
