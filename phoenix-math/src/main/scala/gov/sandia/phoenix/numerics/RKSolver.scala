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

import scala.collection.immutable.SortedMap
import scala.math._

/**
 *===Usage(Scala - Stiff ODE)==={{{
 import gov.sandia.phoenix.numerics._
 import java.io._
 import scala.math._

 def dfdx(t : Double, y : IndexedSeq[Double]) = Array(-1000.0 * y(0) + 3000.0 - 2000.0 * exp(-t))
 val solver = RKDP_8_7.createSolver(dfdx, new UnboundedStepSizeController(0.01))
 val ic = (0.0, Vector(0.0))
 val solution = solver(ic, 4.0, 0.1)

 val pw = new PrintWriter(new FileOutputStream("stiff.csv"), true)
 for(s <- solution) pw.println(s._1 + ", " + s._2(0))
 pw.close
 }}}
 *
 *
 *===Usage(Scala - Simple Rocket Launch Model)==={{{
 import gov.sandia.phoenix.numerics._
 import java.io._
 import scala.math._

 //A very simple rocket model in one dimension.
 val motorforce = 400.0 //N
 val drymass = 10.0 //kg
 val fuelmass = 2.0 //kg
 val burnrate = 0.1 //kg/s
 val simtime = 100.0 //s

 val ic = (0.0, Vector(0, 0, fuelmass)) //pos, vel, fuelmass

 def dydt(t : Double, y : IndexedSeq[Double]) = y(1)
 def dvdt(t : Double, y : IndexedSeq[Double]) = {
 val F = if(y(2) > 0.0) motorforce else 0.0
 F / (drymass + y(2)) - 9.81
 }
 def dmdt(t : Double, y : IndexedSeq[Double]) = if(y(2) > 0.0) -burnrate else 0.0
 def dfdx(t : Double, y : IndexedSeq[Double]) = Array(dydt(t, y),dvdt(t, y),dmdt(t, y))

 val solver = RKDP_8_7.createSolver(dfdx, new BoundedStepSizeController(0.0, 5.0, 0.1))
 //val solver = RK_4.createSolver(dfdx, 0.1)
 val solution = solver(ic, simtime, 1.0)

 val pw = new PrintWriter(new FileOutputStream("rocket.csv"), true)
 for(s <- solution) pw.println(s._1 + ", " + s._2(0) + ", " + s._2(1) + ", " + s._2(2))
 pw.close
 }}}
 *
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
class RKSolver(val df : (Double, IndexedSeq[Double]) => IndexedSeq[Double], 
               val bt : ButcherTableau,
               val controller : StepSizeController) {
  def set(newController : StepSizeController) = new RKSolver(df, bt, newController)
  
  final def next(ic : (Double, IndexedSeq[Double]), dt : Double) =
    for(phi <- ϕ(ic, dt)) yield (ic._1 + dt, for(i <- 0 until ic._2.length) yield ic._2(i) + phi(i))

  def shoot(ic : (Double, IndexedSeq[Double]), f : (Double, IndexedSeq[Double]) => Double, guess : Double) = 
    Numerics.secant(guess, guess - 1.0){ x =>
    val soln = apply(ic, x, 1.0)
    val t = soln.lastKey
    val y = soln(t)
    f(t, y)
  }

  final def apply(ic : (Double, IndexedSeq[Double]), 
                  tf : Double,
                  dti : Double) : SortedMap[Double, IndexedSeq[Double]] = {
    def apply_(current_dt : Double, current_ic : (Double, IndexedSeq[Double]), res : List[(Double, IndexedSeq[Double])]) : List[(Double, IndexedSeq[Double])] =
      if(current_ic._1 >= tf) res else {
        val next_ic = controller.step(current_ic, min(tf - current_ic._1, current_dt), this)
        apply_(next_ic._1, next_ic._2, next_ic._2 :: res)
      }
    SortedMap.empty[Double, IndexedSeq[Double]]++
    (if(ic._1 > tf) Nil else apply_(dti, ic, ic :: Nil))
  }

  final def solve(ti : Double, yi : Array[Double], tf : Double, dti : Double) = this((ti, yi), tf, dti)

  /**
   * Compute the increment values. y(i)+ = y(i) + ϕ(i) * dt. ϕ is a weighted
   * value of the derivatives over the desired time step.
   */
  private final def ϕ(ic : (Double, IndexedSeq[Double]), dt : Double) = {
    val ks = k(ic, dt)
    for(b <- bt.b) yield
      for(fn <- ic._2.indices)
        yield dt*(for(i <- ks.indices) yield b(i)*ks(i)(fn)).sum
  }

  /**
   * Compute several derivatives using the initial conditions. 
   */
  private final def k(ic : (Double, IndexedSeq[Double]), dt : Double) = {
    def k(i : Int, ic : (Double, IndexedSeq[Double]), ks : Array[IndexedSeq[Double]]) : Array[IndexedSeq[Double]] = if(i == bt.c.length) ks else {
      val xs = for(n <- ic._2.indices) yield ic._2(n) + dt*(for(j <- 0 until i) yield bt.a(i)(j) * ks(j)(n)).sum
      ks(i) = df(ic._1 + bt.c(i) * dt, xs)
      k(i + 1, ic, ks)
    }
    k(0, ic, Array.ofDim[IndexedSeq[Double]](bt.c.length))
  }
}
