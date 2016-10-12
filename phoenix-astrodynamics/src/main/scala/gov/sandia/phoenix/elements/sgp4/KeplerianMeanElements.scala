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

package gov.sandia.phoenix.elements.sgp4

import gov.sandia.phoenix.constants.Time
import gov.sandia.phoenix.elements.kepler.KeplerElements
import gov.sandia.phoenix.propagators.sgp4.GravityConstants
import gov.sandia.phoenix.time.JD

case class KeplerianMeanElements(keplers: KeplerElements, epoch : JD, bStarDrag: Double, gc: GravityConstants) extends MeanElements {
  def I0 = keplers.inclination.radians
  def Ω0 = keplers.OMEGA.radians
  def e0 = keplers.e
  def ω0 = keplers.omega.radians
  def meanAnomaly = keplers.meanAnomaly.value
  def meanMotion = keplers.n * Time.SEC_PER_MIN
}