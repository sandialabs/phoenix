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

  sealed abstract class Power {
    def doubleValue: Double

    def build(newValue: Double): Power

    def scale(s: Double) = build(doubleValue * s)

    def watts: Power

    def kilowatts: Power = Kilowatts(watts.doubleValue / 1000)
  }

  case class Watts(doubleValue: Double) extends Power {

    final override def watts = this

    def build(newValue: Double) = Watts(newValue)
  }

  case class Kilowatts(doubleValue: Double) extends Power {

    final override def kilowatts = this

    final override def watts = Watts(doubleValue * 1000)

    def build(newValue: Double) = Kilowatts(newValue)
  }
