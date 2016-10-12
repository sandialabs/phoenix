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

class PenumbraTest extends FunSuite {
  val t = TimeBuilder(2015, 11, 11, 11)
  val occluder = WGS84.sphere
  val source = Sol.sphere(t)
  val penumbra = Penumbra(source, occluder)

  test("Penumbra should not contains its apex, which is between the Earth and the Sun"){
    assert(!penumbra.contains(penumbra.apex))
  }

  test("Penumbra should not contains its apex or any other point between the Earth and the Sun"){
    assert(!penumbra.contains(penumbra.apex.scaled(0.99)))
  }

  test("A point just inside the penumbral cone should be in the cone itself."){
    assert(penumbra.cone.contains(penumbra.apex.scaled(0.99)))
  }

  test("Penumbra should contain its 'almost' anti-apex - the point opposite the cone behind the earth."){
    //The actual anti-apex is indeterminate
    assert(penumbra.cone.contains(penumbra.apex.scaled(0.99)))
  }

  test("Penumbra should contain its occluder (The Earth in this case.)"){
    assert(penumbra.contains(occluder.center))
  }

  test("Penumbra should not contain the light source (The Sun)"){
    assert(!penumbra.contains(source.center))
  }
}