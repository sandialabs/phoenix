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

package gov.sandia.phoenix.sp

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.math._
import gov.sandia.phoenix.solarsystem.Sol
import gov.sandia.phoenix.time.JD

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class DragForce(dragCoefficient : Double = 2.2, areaToMassRatio : Double = 0.02) extends SPForceProvider {
  def acceleration(t : JD, state : ECIStateVector) = {
    val rho = HPDModel(t.ECItoECEF(state.position).toGeodetic.elevation, SolarFlux.fit(t),
      ECItoRaDec(Sol.eciPosition(t)), state)
    Drag.drag(dragCoefficient, areaToMassRatio, rho, state)
  }
}