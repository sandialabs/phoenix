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
 * To create a new area type you must override the method defining your unit (i.e. def unit = this)
 * and a method to convert your unit to a base unit (unless it is a base unit). Note that all unit
 * conversions should be done in a base unit type to prevent infinite recursion.
 */
sealed abstract class Area {
  def doubleValue: Double

  def build(newValue: Double): Area

  def scale(s: Double) = build(doubleValue * s)

  def squareMeters: Area

  def squareKilometers: Area = SquareKilometers(squareMeters.doubleValue / 1.0E6)

}

case class SquareMeters(doubleValue : Double) extends Area {
  final override def squareMeters = this

  def build(newValue : Double) = SquareMeters(newValue)
}

case class SquareKilometers(doubleValue : Double) extends Area {
  final override def squareKilometers = this

  final override def squareMeters = SquareMeters(doubleValue * 1.0E6)

  def build(newValue : Double) = SquareKilometers(newValue)
}