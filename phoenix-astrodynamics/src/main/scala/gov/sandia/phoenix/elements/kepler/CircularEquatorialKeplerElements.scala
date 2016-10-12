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

package gov.sandia.phoenix.elements.kepler

import gov.sandia.phoenix.elements.kepler.KeplerElements._
import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry._

case class CircularEquatorialKeplerElements(p : Double, lambda_true : Angle) extends KeplerElements {
  val e = 0.0
  val inclination = Angle.ZERO
  val eccentricAnomaly = ECCENTRIC_ANOMALY(lambda_true.radians)
  val OMEGA = Angle.NaN
  val omega = Angle.NaN
  val flightPathAngle = Angle.ZERO
  val nu = Angle.NaN

  override lazy val apogee = new CircularEquatorialKeplerElements(p, Angle.Pi).state
  override lazy val perigee = new CircularEquatorialKeplerElements(p, Angle.ZERO).state
  override lazy val ascendingNode = new CircularEquatorialKeplerElements(p, lambda_true.explement).state
  override lazy val descendingNode = new CircularEquatorialKeplerElements(p, lambda_true.supplement).state
  lazy val orientation = q(Angle.ZERO, inclination, Angle.ZERO)
  
  override def state : ECIStateVector = RandV(p, e, inclination, Angle.ZERO, Angle.ZERO, lambda_true)
  override def state(dt : Double) : ECIStateVector = new CircularEquatorialKeplerElements(p, nu(dt)).state

  override def pretty = super.pretty + "\ntrue longitude: " + lambda_true.degrees + "\u00B0"
}
