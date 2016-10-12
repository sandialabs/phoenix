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

object RightAscension {
  def fromDegrees(degrees : Double) : RightAscension = if(degrees < 0) fromDegrees(degrees + 360) else {
    val dhours = degrees / 15
    val hours = dhours.toInt
    val dmin = (dhours - hours) * 60
    val min = dmin.toInt
    val sec = (dmin - min) * 60
    new RightAscension(hours, min, sec)
  }
  def fromRadians(radians : Double) = fromDegrees(radians.toDegrees)
  
  def correct(h : Int) : Int = if(h < 0) correct(h + 24) else if(h > 23) correct(h - 24) else h
}

case class RightAscension(h : Int, minutes : Int, seconds : Double) {
  val hours = RightAscension.correct(h)
  lazy val degrees = 15 * hours + (minutes + seconds / 60.0) / 4.0
  lazy val radians = degrees.toRadians
  def pretty = hours + "\u02B0 " + minutes + "\u1D50 " + seconds + "\u02E2"
}
