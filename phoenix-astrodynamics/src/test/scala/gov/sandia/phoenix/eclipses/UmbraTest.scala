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

package gov.sandia.phoenix.eclipses

import gov.sandia.phoenix.constants.WGS84
import gov.sandia.phoenix.solarsystem.Sol
import gov.sandia.phoenix.time.TimeBuilder
import org.scalatest.FunSuite

class UmbraTest extends FunSuite {
  val t = TimeBuilder(2015, 11, 11, 11)
  val occluder = WGS84.sphere
  val source = Sol.sphere(t)
  val umbra = Umbra(source, occluder)

  test("Umbra should contain a point just inside its apex."){
    assert(umbra.contains(umbra.apex.scaled(0.99)))
  }

  test("Umbra should contain an Earth surface point opposite the Sun"){
    assert(umbra.contains(umbra.apex.normalized.scaled(WGS84.R_EQ_M)))
  }

  test("Umbra should not contain an Earth surface point facing the Sun"){
    assert(!umbra.contains(umbra.apex.normalized.negated.scaled(WGS84.R_EQ_M)))
  }

  test("Umbra should not contain its anti-apex"){
    assert(!umbra.contains(umbra.apex.negated))
  }

  test("Umbra should not contain its occluder (The Earth in this case.)"){
    assert(!umbra.contains(occluder.center))
  }

  test("Umbra should not contain the light source (The Sun)"){
    assert(!umbra.contains(source.center))
  }
}