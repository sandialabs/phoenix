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

package gov.sandia.phoenix.numerics

import scala.math._

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
abstract class StepSizeController {
  def step(ic : (Double, IndexedSeq[Double]), dt : Double, solver : RKSolver) : (Double, (Double, IndexedSeq[Double]))
}

object BoundedStepSizeController {
  final def error(solns : IndexedSeq[(Double, Seq[Double])]) = sqrt((solns.head._2 zip solns(1)._2).map(x=>(x._1-x._2)*(x._1-x._2)).sum)
}

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
class BoundedStepSizeController(val minimum : Double, val maximum : Double, val tol : Double) extends StepSizeController {
  def newMinimum(newMin : Double) = new BoundedStepSizeController(newMin, max(maximum, newMin), tol)
  def newMaximum(newMax : Double) = new BoundedStepSizeController(min(minimum, newMax), newMax, tol)
  private final def control(suggested : Double) = min(maximum, max(minimum, suggested))
  final def adjustDt(error : Double, dt : Double, solver : RKSolver) = if(error == 0.0) dt else 0.9 * pow(tol / error, 1.0 / (solver.bt.order + 1)) * dt
  
  def step(ic : (Double, IndexedSeq[Double]), step_size : Double, solver : RKSolver) : (Double, (Double, IndexedSeq[Double])) = {
    val solutions = solver.next(ic, step_size)
    val err = BoundedStepSizeController.error(solutions)
    val ndt = control(adjustDt(err, step_size, solver))
    if(err < tol) (ndt, solutions.head) else step(ic, ndt, solver)
  }
}

class UnboundedStepSizeController(tol : Double) extends BoundedStepSizeController(0.0, Double.PositiveInfinity, tol)

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
object FixedStepSizeController extends StepSizeController {
  def step(ic : (Double, IndexedSeq[Double]), step_size : Double, solver : RKSolver) = (step_size, solver.next(ic, step_size)(0))
  def getInstance = this
}

//  final def adjustDt(error : Double, dt : Double) = if(error == 0.0) dt else 0.9 * pow(tol / error, 1.0 / (bt.order + 1)) * dt
//
//  final def step(ic : (Double, IndexedSeq[Double]), 
//                 dt : Double) : (Double, (Double, IndexedSeq[Double])) = bt.adaptive match {
//    case false => (dt, next(ic, dt)(0))
//    case true => {
//        val next_ic = next(ic, dt)
//        val err = error(next_ic)
//        val ndt = adjustDt(err, dt)
//        if(err < tol) (ndt, next_ic(0)) else step(ic, ndt)
//      }
//  }