package gov.sandia.phoenix.access

import gov.sandia.phoenix.geometry.Geodetic
import gov.sandia.phoenix.propagators.Propagator
import gov.sandia.phoenix.time.Interval

object PassUtil {
  def maxElevation(geo : Geodetic, propagator : Propagator, interval : Interval) = {
    val frame = geo.toSEZFrame
    val (maxTime, maxElevation) = gov.sandia.phoenix.numerics.optimization.goldenMax(0.0, 1.0, 0.01){ x =>
      val t = interval.interpolate(x)
      (propagator.position(t) map { apos =>
        frame.toAzElR(t.ECItoECEF(apos)).elevation
      }).getOrElse(Double.NegativeInfinity)
    }
    (interval.interpolate(maxTime), maxElevation)
  }
}
