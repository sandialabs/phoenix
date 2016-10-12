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

object Numerics {
  def interpolate(x0 : Double, y0 : Double, x1 : Double, y1 : Double, x : Double) = y1 + (y1 - y0) / (x1 - x0) * (x - x0)
  
  //Error metrics
  def pctDiff(x0 : Double, x1 : Double) = if(x0 == x1) 0.0 else 100 * abs(2.0 * (x1 - x0) / (x1 + x0))
  def pctErr(computed : Double, actual : Double) = abs((computed - actual) / actual)
  def relErr(rNew : Double, rOld : Double) = 100*abs((rNew - rOld) / rNew)
  
  //Closed methods
  private def midFunc(lo : Double, flo : Double, hi : Double, fhi : Double) : Double = (lo + hi) * 0.5
  def bisect = bracket(midFunc)_
  
  private def falsePosFunc(lo : Double, flo : Double, hi : Double, fhi : Double) : Double = hi - fhi * (lo - hi) / (flo - fhi)
  def falsepos = bracket(falsePosFunc)_
  
  def bracket(fmid : (Double, Double, Double, Double) => Double)
  (f : Double => Double, lo : Double, hi : Double) : Double =
    bracket(f, lo, f(lo), hi, f(hi), lo, fmid)
  
  private final def bracket(f : Double => Double, 
                            lo : Double, flo : Double, hi : Double, fhi : Double, oldmid : Double, 
                            midfcn : (Double, Double, Double, Double) => Double) : Double =
  {
    //println(flo * fhi + " " + lo + " " + hi)
    require(flo*fhi <= 0.0 && (lo <= hi))
    
    val mid = midfcn(lo, flo, hi, fhi)
    if(relErr(mid, oldmid) < 1E-10)
      mid
    else
    {
      val fmid = f(mid)
      if(fmid * flo < 0.0)
        bracket(f, lo, flo, mid, fmid, mid, midfcn)
      else if(fmid * fhi < 0.0)
        bracket(f, mid, fmid, hi, fhi, mid, midfcn)
      else
        mid
    }
  }
  
  //Section of open methods
  def fixedPoint(x : Double)(f : Double => Double) : Double = fixedPoint(f(x), x)(f)
  private final def fixedPoint(x : Double, xOld : Double)(f : Double => Double) : Double = if(relErr(x, xOld) < 1E-10) x else fixedPoint(f(x), x)(f)
  
  def secant(x : Double, xminus : Double)(f : Double => Double) : Double = secant(x, f(x), xminus, f(xminus))(f)
  
  private final def secant(x : Double, fx : Double, xminus : Double, fxminus : Double, iter : Int = 0, maxIter : Int = 10)
  (f : Double => Double) : Double = if(iter == maxIter) x else {
    if(fx - fxminus == 0 && relErr(x, xminus) < 1E-5) x else {
      val xplus = x - (fx * (x - xminus)) / (fx - fxminus)
      if(relErr(xplus, x) < 1E-10) xplus else secant(xplus, f(xplus), x, fx, iter+1)(f)
    }
  }
  
  def gradientSearch(p : (Double, Double, Double), f : (Double, Double) => Double) : (Double, Double, Double) = {
    val delta = 0.00001
    val heights = for(i <- -1 to 1 by 2; j <- -1 to 1 by 2; x = i * delta; y = j * delta) 
      yield (p._1+x, p._2+y, f(p._1+x, p._2+y))
    val best = heights reduceLeft ((a, b)=>if(a._3 > b._3) a else b)
    if(p._3 >= best._3) p else gradientSearch(best, f)
  }

  def iterate(f : Double => Double, pctDiffTol : Double = 1E-4,
              maxIter : Int = 20, x_old : Double = Double.PositiveInfinity,
              iter : Int = 0)(x_new : Double) : Double = {
    if(pctDiff(x_new, x_old) < pctDiffTol || iter > maxIter) x_new else
      iterate(f, pctDiffTol, maxIter, x_new, iter + 1)(f(x_new))
  }

  def secantStep(x : Double, xold : Double)(f : Double => Double) = x - f(x) * (x - xold) / (f(x) - f(xold))

//  def secantMethod(guess : Double)(f : Double => Double) = {
//    val guess2 = guess + 1
//    def step(x : Double, fx : Double, xo : Double, fxo : Double) : Double = if(abs(x - xo) < 1.0E-5) { x } else {
//      val xn = x - fx * (x - xo) / (fx - fxo)
//      step(xn, f(xn), x, fx)
//    }
//    step(guess, f(guess), guess2, f(guess2))
//  }

  def newtonStep(x : Double, f : Double => Double, df : Double => Double) = x - f(x) / df(x)
  def newtonStep(x : Double, f_over_df : Double => Double) = x - f_over_df(x)
}
