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
import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.propagators.sgp4.GravityConstants
import gov.sandia.phoenix.time.JD

case class ECIStateVectorMeanElements(state: ECIStateVector, epoch: JD, bStarDrag: Double, gc: GravityConstants) extends MeanElements {
  def I0 = state.keplers.inclination.radians
  def Ω0 = state.keplers.OMEGA.radians
  def e0 = state.keplers.e
  def ω0 = state.keplers.omega.radians
  def meanAnomaly = state.keplers.meanAnomaly.value
  def meanMotion = state.keplers.n * Time.SEC_PER_MIN
}