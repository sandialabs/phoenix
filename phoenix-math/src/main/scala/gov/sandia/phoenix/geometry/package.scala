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
