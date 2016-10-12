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
