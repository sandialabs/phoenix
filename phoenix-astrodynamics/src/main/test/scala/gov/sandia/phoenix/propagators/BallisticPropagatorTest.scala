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
