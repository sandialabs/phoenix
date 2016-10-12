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

package gov.sandia.phoenix.elements.tle

object TLEFilters {
  val geo = (tle : TLE) => tle.meanMotion * TLEUtils.MINUTES_PER_REVOLUTION match {
    case m if m >= 0.9 && m <= 1.1 => true
    case _ => false
  }

  val debris = (tle : TLE) => tle.name contains " DEB"
  val rocket_body = (tle : TLE) => tle.name contains " R/B"

  def inclination(lo : Double, hi : Double) = { (tle : TLE) => tle.inclination <= lo && tle.inclination >= hi }
}
