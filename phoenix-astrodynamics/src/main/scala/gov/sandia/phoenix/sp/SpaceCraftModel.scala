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
import gov.sandia.phoenix.geometry.Vector3
import gov.sandia.phoenix.numerics._
import gov.sandia.phoenix.propagators.Propagator
import gov.sandia.phoenix.time.JD

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
class SpaceCraftModel(val epoch : JD,
                      val initialState : ECIStateVector, 
                      val surfaceArea : Double,
                      val dryMass : Double, 
                      val fuelMass : Double,
                      val ṁ : Double,
                      val forceMag : Double,
                      val ε : Double,
                      val dragCoefficient : Double,
                      val maneuvers : Set[DeltaVForce] = Set.empty) extends Propagator {
  val controller = new UnboundedStepSizeController(1.0)
  val solver = RKDP_8_7.createSolver(dfdx, controller)
  val amr = surfaceArea / (dryMass + fuelMass)
  
  val forcemodel = SMGModel+SRP(amr, ε)+DragForce(dragCoefficient, amr)
  
  val initialConditions = (0.0, Array.tabulate(7){ index =>
      if(index == 6) fuelMass else initialState.toArray(index)
    } : IndexedSeq[Double])

  def state(t : JD) = Some {
    val solution = solve(t)
    val lastDt = solution.lastKey
    val s = solution(lastDt)
    ECIStateVector(Vector3(s(0), s(1), s(2)), Vector3(s(3), s(4), s(5)))
  }
  
  def solve(t : JD)= {
    val dt = if(t > epoch) (epoch until t).getDurationSeconds else -(t until epoch).getDurationSeconds
    solver(initialConditions, dt.doubleValue, 600)
  }
  
  def dfdx(t : Double, y : IndexedSeq[Double]) = {
    val state = ECIStateVector(Vector3(y(0), y(1), y(2)), Vector3(y(3), y(4), y(5)))
    val f = forcemodel ΣF (epoch plusSeconds t, state)
    Array(y(3), y(4), y(5), f.x, f.y, f.z, -ṁ)
  }
}