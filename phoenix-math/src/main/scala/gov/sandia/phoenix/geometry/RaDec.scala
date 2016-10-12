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

/**
 * Right Ascension and Declination, both in degrees
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class RaDec(rightAscension : RightAscension, declination : Declination) {
  override def toString = rightAscension + ", " + declination

  def mid(that : RaDec) = 
    new RaDec(RightAscension.fromDegrees((this.rightAscension.degrees + that.rightAscension.degrees) * 0.5), 
        Declination.fromDegrees((this.declination.decimalDegrees + that.declination.decimalDegrees) * 0.5))
  
  def toECI = Radians(rightAscension.radians).rz * Radians(-declination.radians).ry * X_AXIS
}