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

package gov.sandia.phoenix.elements.gps.eph

import gov.sandia.phoenix.elements.gps.{OrbitalElements, GPSElements}

/**
 * An Ephemeris is a type of orbital element.  In terms of accuracy,
 * Ephemeris is better than Almanac is better than Two Line Elements when you
 * are near the time of applicability (epoch) and is worse as you get further away
 * from the epoch.
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 *
 */
trait Ephemeris extends OrbitalElements with GPSElements
{
  def meanAnomaly : Double
  def meanMotionDifference : Double
  def eccentricity : Double
  def squareRootOfSemiMajorAxis : Double
  def semiMajorAxis : Double
  def longitudeOfAscendingNode : Double
  def inclination : Double
  def argumentOfPerigee : Double
  def rateOfRightAscension : Double
  def rateOfInclination : Double
  def cuc : Float
  def cus : Float
  def crc : Float
  def crs : Float
  def cic : Float
  def cis : Float
  def timeOfEphemeris : Float
  def issueOfData : Byte
  def L2Code : Byte
  def weekNumber : Int
  def L2PDataFlag : Boolean
  def SVAccuracy : Byte
  def SVHealth : Byte
  def tgd : Float
  def issueOfDataClock : Int
  def timeOfCollection : Float
  def af0 : Float
  def af1 : Float
  def af2 : Float
}