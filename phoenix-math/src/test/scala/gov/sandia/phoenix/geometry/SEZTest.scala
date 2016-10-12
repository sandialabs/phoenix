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


class SEZTest extends FunSuite
{
  test("straightUp") {
    val location = Geodetic(0.0, 45.0, 1000)
    val ecef = location.toECEF
    val above = ecef * 1000.0
    val sez = location.toSEZFrame
    val azelr = sez.toAzElR(above)
    //This won't be exactly 0 because the SEZ frame uses an ellipsoid to compute the frame.
    assert(90.0 - azelr.elevation < 0.2)
  }

  test("straightUp @ (0,0,0)") {
    val location = Geodetic(0, 0, 0)
    val ecef = location.toECEF
    val above = ecef * 1000.0
    val sez = location.toSEZFrame
    val azelr = sez.toAzElR(above)
    //This won't be exactly 0 because the SEZ frame uses an ellipsoid to compute the frame.
    assert(90.0 - azelr.elevation < 0.2)
  }
}