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

package gov.sandia.phoenix.elements.gps.alm

import gov.sandia.phoenix.constants._


import gov.sandia.phoenix.elements.gps.{OrbitalElements, GPSElements}

/**
 * An almanac is a type of orbital element.  In terms of accuracy,
 * Ephemeris is better than Almanac is better than Two Line Elements when you
 * are near the time of applicability (epoch) and is worse as you get further away
 * from the epoch.
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 *
 */
trait Almanac extends OrbitalElements with GPSElements {
  def eccentricity : Double
  def timeOfApplicability : Float
  def inclination : Double
  def rateOfRightAscension : Double
  def longitudeOfAscendingNode : Double
  def squareRootOfSemiMajorAxis : Double
  def argumentOfPerigee : Double
  def meanAnomaly : Double
  def af0 : Float
  def af1 : Float
  def weekNumber : Int
  def epochUTC=(weekNumber + 1024.0) * Time.SEC_PER_WEEK + timeOfApplicability
  def semiMajorAxis=this.squareRootOfSemiMajorAxis * this.squareRootOfSemiMajorAxis
  def cic= 0.0f
  def cis= 0.0f
  def crc= 0.0f
  def crs= 0.0f
  def cuc= 0.0f
  def cus= 0.0f
  def meanMotionDifference= 0.0
  def rateOfInclination = 0.0
}
