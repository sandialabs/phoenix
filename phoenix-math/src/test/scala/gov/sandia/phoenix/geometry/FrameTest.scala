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

class FrameTest extends FunSuite {
  test("SEZ") {
    val v = new Vector3(0, 1, 5)
    val f = new Frame(Angle.D90.rx, new Vector3(0, 10, 0))
    //The fact that you are going to SEZ implies an inverse transformation.
    val res = f.toSEZ(v)
    assert(abs(res.x - 0) < 1E-10)
    assert(abs(res.y - 5) < 1E-10)
    assert(abs(res.z - 9) < 1E-10)
  }
            
  test("Help in understanding") {
    val v = new Vector3(0, 0, 1)
    val f = new Frame(Angle.D90.rx, new Vector3(0, 1, 0))
        
    //The point v is in the f frame. The result puts it in the base frame.
    val res = f * v
    assert(abs(res.x) < 1E-10)
    assert(abs(res.y) < 1E-10)
    assert(abs(res.z) < 1E-10)
  }
}