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

class AzElTest extends FunSuite {
  //Ascending non-crossover
  test("30 to 270") {
    val lo = new AzEl(30, 0)
    val hi = new AzEl(270, 0)
    val res = lo.interpolate(0.5, hi)
    assert(res.azimuth === 150)
  }

  //Descending non-crossover
  test("270 to 30") {
    val lo = new AzEl(270, 0)
    val hi = new AzEl(30, 0)
    val res = lo.interpolate(0.5, hi)
    assert(res.azimuth === 150)
  }

  //Descending with crossover
  test("30 to -90") {
    val lo = new AzEl(30, 0)
    val hi = new AzEl(-90, 0)
    val res = lo.interpolate(0.5, hi)
    assert(res.azimuth === -30)
  }

  //Ascending with crossover
  test("-90 to 30") {
    val lo = new AzEl(-90, 0)
    val hi = new AzEl(30, 0)
    val res = lo.interpolate(0.5, hi)
    assert(res.azimuth === -30)
  }


}
