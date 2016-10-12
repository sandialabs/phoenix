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

object Declination {
  def fromDegrees(decimalDegrees : Double) = {
    val degrees = decimalDegrees.toInt
    val dmin = (decimalDegrees - degrees) * 60
    val min = dmin.toInt
    val sec = (dmin - min) * 60
    new Declination(degrees, min, sec)
  }
  def fromRadians(radians : Double) = fromDegrees(radians.toDegrees)
}

case class Declination(degrees : Int, minutes : Int, seconds : Double) {
  def decimalDegrees = degrees + (minutes + seconds / 60.0) / 60.0
  def radians = decimalDegrees.toRadians
  
  override def toString = (if(degrees >= 0) "+" else "") +
  degrees + "\u00B0" + minutes + "'" + seconds + "\""
}
