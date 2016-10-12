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

import scala.math._

object AzEls {
  /**
   * Compute the sum of angles traversed in azimuth for an indexed set of azels assumed to form a closed spherical polygon.
   * The sum will be 0 or +/-360. 0 corresponds to a polygon not containing either pole or both. +/-360 wraps a single
   * pole. The sign is based on winding. +360 winds in the direction of azimuth, -360 winds opposite (and would be
   * positive from a right-handedness perspective).
   */
  def angularSum(azels : IndexedSeq[AzEl]) = (azels.indices map { i =>
    (Degrees(azels((i + 1) % azels.length).azimuth) - Degrees(azels(i).azimuth)).constrainSigned.degrees }).sum

  /**
   * Test if an indexed sequence of AzEls contains a single pole. Note that we cannot determine which pole without
   * some assumptions.
   */
  def dividesPoles(azels : IndexedSeq[AzEl]) = abs(angularSum(azels)) > 359.9

  def guessExteriorPoint(azels : IndexedSeq[AzEl]) = AzEls.angularSum(azels) match {
    //If the point does not contain a pole, pick either pole
    case non_polar if abs(non_polar) <= 1.0E-6 =>  Z_AXIS
    //If the winding is positive (along azimuth) we assume we can see "up" therefore we can't see "down"
    case polar if polar > 0 => -Z_AXIS
    //The reverse of the above is true regarding winding.
    case polar => Z_AXIS
  }
}
