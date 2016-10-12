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

package gov.sandia.phoenix.uom

sealed abstract class Speed {
  def doubleValue : Double

  def feetPerSecond : Speed = FeetPerSecond(Meters(metersPerSecond.doubleValue).feet.doubleValue)
  def metersPerSecond : Speed = MetersPerSecond(Feet(feetPerSecond.doubleValue).meters.doubleValue)

  def knots : Speed = Knots(kilometersPerHour.doubleValue / 1.852)
  def milesPerHour : Speed = MilesPerHour(feetPerSecond.doubleValue * 3600 / 5280)
  def kilometersPerHour : Speed = KilometersPerHour(metersPerSecond.doubleValue * 3.6)

  def distance(time : Time) : Distance = Meters(metersPerSecond.doubleValue * time.seconds.doubleValue)
}

case class FeetPerSecond(doubleValue : Double) extends Speed {
  final override def feetPerSecond = this
}

case class MetersPerSecond(doubleValue : Double) extends Speed {
  final override def metersPerSecond = this
}

case class MilesPerHour(doubleValue : Double) extends Speed {
  final override def milesPerHour = this
  final override def feetPerSecond = FeetPerSecond(doubleValue * 5280 / 3600)
}

case class KilometersPerHour(doubleValue : Double) extends Speed {
  final override def kilometersPerHour = this
  final override def metersPerSecond = MetersPerSecond(doubleValue / 3.6)
}

case class Knots(doubleValue : Double) extends Speed {
  final override def knots = this
  final override def metersPerSecond = MetersPerSecond(doubleValue * 1852.0 / 3600.0)
}

case class UserDefinedSpeed(distance : Distance, time : Time) extends Speed {
  def doubleValue = distance.doubleValue / time.doubleValue
  final override def metersPerSecond = MetersPerSecond(distance.meters.doubleValue / time.seconds.doubleValue)
}