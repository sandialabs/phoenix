package gov.sandia.phoenix

import scala.math._

/**
 * Note: Implementation of this grid would be awesome.
 * http://www.realtimerendering.com/intersections.html
 */
package object geometry {
  final def constrainAzimuth(x : Double) : Double = x match {
    case a if x > 360.0 => constrainAzimuth(a - 360.0)
    case a if x < 0.0 => constrainAzimuth(a + 360.0)
    case a => a
  }

  final def constrainElevation(x : Double) = min(90.0, max(-90.0, x))
}
