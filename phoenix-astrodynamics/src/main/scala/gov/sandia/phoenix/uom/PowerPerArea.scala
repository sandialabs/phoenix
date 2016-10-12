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


sealed abstract class PowerPerArea {

  def doubleValue : Double

  def wattsPerSquareMeter: PowerPerArea

  def kilowattsPerSquareMeter: PowerPerArea = WattsPerSquareMeter(wattsPerSquareMeter.doubleValue / 1000)
}

case class WattsPerSquareMeter(doubleValue : Double) extends PowerPerArea {

  final override def wattsPerSquareMeter = this

  def this(power: Power, area : Area)  =
    this(power.watts.doubleValue / area.squareMeters.doubleValue)
}

case class KilowattsPerSquareMeter(doubleValue : Double) extends PowerPerArea {

  final override def kilowattsPerSquareMeter = this

  final override def wattsPerSquareMeter = WattsPerSquareMeter(doubleValue * 1000)
}

case class Irradiance(doubleValue: Double) extends PowerPerArea {

  def this(power: Power,   area : Area)  =
    this(power.watts.doubleValue / area.squareMeters.doubleValue)

  final override def wattsPerSquareMeter = this

  final override def kilowattsPerSquareMeter = KilowattsPerSquareMeter(doubleValue)
}

/**
 * Example of alternate class for Irradiance. A companion object is defined for the
 * auxiliary constructor in order to keep the same syntax (i.e. no 'new' keyword) when
 * instantiating Irradiance objects from doubleValue.
 */
case class AlternateIrradiance(power: Power, area: Area) extends PowerPerArea {

  def doubleValue = power.watts.doubleValue / area.squareMeters.doubleValue

  final override def wattsPerSquareMeter =
    WattsPerSquareMeter(power.watts.doubleValue / area.squareMeters.doubleValue)

  final override def kilowattsPerSquareMeter = KilowattsPerSquareMeter(doubleValue)
}

/*
object AlternateIrradiance {
  def apply(doubleValue: Double) = AlternateIrradiance(Watts(doubleValue), SquareMeters(1))
}
*/

/**
 * This class has implementation identical to Irradiance but it represents a different
 * physical concept.
 */
case class RadiantExitance(doubleValue: Double) extends PowerPerArea {

  def this(power: Power,   area : Area)  =
    this(power.watts.doubleValue / area.squareMeters.doubleValue)

  final override def wattsPerSquareMeter = this

  final override def kilowattsPerSquareMeter = KilowattsPerSquareMeter(doubleValue)
}

/**
 * This class has implementation identical to Irradiance but it represents a different
 * physical concept.
 */
case class RadiantEmittance(doubleValue: Double) extends PowerPerArea {

  def this(power: Power,   area : Area)  =
    this(power.watts.doubleValue / area.squareMeters.doubleValue)

  final override def wattsPerSquareMeter = this

  final override def kilowattsPerSquareMeter = KilowattsPerSquareMeter(doubleValue)
}

