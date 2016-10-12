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

package gov.sandia.phoenix.propagators

import gov.sandia.phoenix.elements.ballistic.MinimumEnergyTrajectoryGenerator
import gov.sandia.phoenix.geometry.Geodetic
import gov.sandia.phoenix.time.TimeBuilder
import org.scalatest.FunSuite

class BallisticPropagatorTest extends FunSuite {
  test("correct endpoints"){
    val startLocation = Geodetic(0, 0, 0)
    val endLocation = Geodetic(45, 45, 0)
    val trajectory = MinimumEnergyTrajectoryGenerator.fromStartTime(startLocation, endLocation, TimeBuilder(2014, 1, 1))
    assert(startLocation.flatEarthDistance(trajectory.srcGeo) <= 0.1)
    assert(endLocation.flatEarthDistance(trajectory.dstGeo) <= 0.1)
  }
}
