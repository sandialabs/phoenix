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

/**
 * To create a new distance type you must override the method defining your unit (i.e. def unit = this) and a method
 * to convert your unit to a base unit (unless it is a base unit). Note that all unit conversions should be done in a
 * base unit type to prevent infinite recursion.
 */
sealed abstract class Distance {
  def doubleValue : Double

  def build(newValue : Double) : Distance
  def scale(s : Double) = build(doubleValue * s)

  def inches : Distance = Inches(feet.doubleValue * 12.0)
  def feet : Distance = Feet(meters.doubleValue / 0.3048)
  def yards : Distance = Yards(feet.doubleValue / 3.0)
  def miles : Distance = Miles(feet.doubleValue / 5280.0)

  def microns : Distance = Microns(meters.doubleValue * 1E6)
  def meters : Distance = Meters(feet.doubleValue * 0.3048)
  def kilometers : Distance = Kilometers(meters.doubleValue / 1000)

  def nauticalMiles : Distance = NauticalMiles(meters.doubleValue / 1852.0)

  def per(t : Time) = UserDefinedSpeed(this, t)

  def * (x : Double) = build(doubleValue * x)
  def / (x : Double) = build(doubleValue / x)
  def + (that : Distance) = Meters(this.meters.doubleValue + that.meters.doubleValue)
  def / (that : Distance) = this.meters.doubleValue / that.meters.doubleValue
  def / (that : Speed) = Seconds(this.meters.doubleValue / that.metersPerSecond.doubleValue)
  def min(that : Distance) = if(this.meters.doubleValue < that.meters.doubleValue) this else that
  def max(that : Distance) = if(this.meters.doubleValue > that.meters.doubleValue) this else that
}

case class Microns(doubleValue : Double) extends Distance {
  final override def meters = Meters(doubleValue * 1E-6)
  final override def microns = this
  def build(newValue : Double) = Microns(newValue)
}

case class Meters(doubleValue : Double) extends Distance {
  final override def meters = this
  def build(newValue : Double) = Meters(newValue)
}

case class Kilometers(doubleValue : Double) extends Distance {
  final override def meters = Meters(doubleValue * 1000)
  final override def kilometers = this
  def build(newValue : Double) = Kilometers(newValue)
}

case class NauticalMiles(doubleValue : Double) extends Distance {
  final override def meters = Meters(doubleValue * 1852)
  final override def nauticalMiles = this
  def build(newValue : Double) = NauticalMiles(newValue)
}

case class Inches(doubleValue : Double) extends Distance {
  final override def inches = this
  final override def feet = Feet(doubleValue / 12.0)
  def build(newValue : Double) = Inches(newValue)
}

case class Feet(doubleValue : Double) extends Distance {
  final override def feet = this
  def build(newValue : Double) = Feet(newValue)
}

case class Yards(doubleValue : Double) extends Distance {
  final override def feet = Feet(doubleValue * 3.0)
  final override def yards = this
  def build(newValue : Double) = Yards(newValue)
}

case class Miles(doubleValue : Double) extends Distance {
  final override def feet = Feet(doubleValue * 5280)
  final override def miles = this
  def build(newValue : Double) = Miles(newValue)
}

object Foot extends Feet(1)

