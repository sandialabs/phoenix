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
    
object optimization {
  val GOLDEN_MEAN = (sqrt(5.0) - 1.0) * 0.5

  val goldenMax = golden(_>_)_
  val goldenMin = golden(_<_)_

  private final def golden(func : (Double, Double) => Boolean)
  (li : Double, ui : Double, tol : Double)(f : Double => Double) : (Double, Double) = {
      
    def goldenSearch(l : (Double, Double), u : (Double, Double), x1 : (Double, Double),
                     x2 : (Double, Double)) : (Double, Double) = {
      if(Numerics.pctDiff(u._1, l._1) < tol) {
        val x = (u._1 + l._1) * 0.5
        (x, f(x))
      } else if(func(x2._2, x1._2)) {
        val nx = x1._1 - GOLDEN_MEAN * (x1._1 - l._1)
        goldenSearch(l, x1, x2, (nx, f(nx)))
      } else {
        val nx = x2._1 + GOLDEN_MEAN * (u._1 - x2._1)
        goldenSearch(x2, u, (nx, f(nx)), x1)
      }
    }
      
    if(li > ui) golden(func)(ui, li, tol)(f) else {
      val d = GOLDEN_MEAN * (ui - li)
      val x1 = li + d
      val x2 = ui - d
      goldenSearch((li, f(li)), (ui, f(ui)), (x1, f(x1)), (x2, f(x2)))
    }
  }

  /**
   * Find an extreme on the boundary [lo, hi] for the function f(x). Note that the extreme may be a max or a min.
   */
  def quadratic(lo : Double, hi : Double)(f : Double => Double) = {
    val mid = (lo + hi) * 0.5

    def step(x0 : Double, fx0 : Double, x1 : Double, fx1 : Double, x2 : Double, fx2 : Double) : (Double, Double) =
      if(Numerics.pctDiff(x0, x2) < 0.01)
        (x1, fx1)
      else {
      val num = fx0 * (x1 * x1 - x2 * x2) + fx1 * (x2 * x2 - x0 * x0) + fx2 * (x0 * x0 - x1 * x1)
      val den = 2.0 * (fx0 * (x1 - x2) + fx1 * (x2 - x0) + fx2 * (x0 - x1))
      val x3 = num / den
      if(x3 <= x1)
        step(x0, fx0, x3, f(x3), x1, fx1)
      else if(x3 >= x1)
        step(x1, fx1, x3, f(x3), x2, fx2)
      else
        (x1, fx1)
    }

    step(lo, f(lo), mid, f(mid), hi, f(hi))
  }
}