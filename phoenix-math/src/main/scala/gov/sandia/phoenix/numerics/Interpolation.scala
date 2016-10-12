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

object Interpolation {
  def main(args : Array[String]) : Unit = {
    println("Interpolation using Lagrange Polynomials")
    val xi = Array(1.0, 4.0, 6.0)
    val fx = for(x <- xi) yield log(x)
    println(new LagrangePolynomial(xi, fx, 0)(2))
    println(new LagrangePolynomial(xi, fx, 1)(2))
    println(new LagrangePolynomial(xi, fx, 2)(2))
    println(new LagrangePolynomial(xi, fx, 3)(2))
    println(new LagrangePolynomial(xi, fx)(2))
    
    val x = for(i <- 0 until 1000; c = i - 500; t = c / 500.0; r = t * Pi) yield r
    val fcx = for(r <- x) yield cos(r)
    val f = new LagrangePolynomial(x.toArray, fcx.toArray)
    println(cos(0.358304))
    println(f(0.358304))
    
//    println("Run the thomas algorithm for tridiagonal decomposition")
//    testThomas
    
    println("Interpolation using cubic splines")
    testCubicSpline
  }
  
  def testCubicSpline = {
    val x = Array(3.0, 4.5, 7.0, 9.0)
    val fx = Array(2.5, 1.0, 2.5, 0.5)
    val cs = new CubicSpline(x, fx)
    for(d2 <- cs.d2x) println(d2)
    for(res <- cs(5.0)) println(res)
  }
  
  def testThomas = {
    val e = Array(Double.NaN, -1, -1, -1)
    val f = Array(2.04, 2.04, 2.04, 2.04)
    val g = Array(-1, -1, -1, Double.NaN) 
    val r = Array(40.8, 0.8, 0.8, 200.8)
    
    for(x <- Thomas(e, f, g, r)) println(x)
  }

  def linterp(x0 : Double, y0 : Double, x1 : Double, y1 : Double, x : Double) = y0 + (y1 - y0) / (x1 - x0) * (x - x0)
}

class LagrangePolynomial(val xi : Array[Double], val fi : Array[Double], val terms : Int) {
  require(terms <= xi.length)
  
  def this(xi : Array[Double], yi : Array[Double]) = this(xi, yi, xi.length)
  private final def L(x : Double, i : Int, lx : Array[Double]) = 
    (1.0 /: (for(j <- 0 until terms; if(j != i)) yield (x - lx(j))/(lx(i) - lx(j))))(_*_)
  def apply(x : Double) = {
    val index = (1 until xi.length).find(i => (x >= xi(i-1) && x <= xi(i)))
    val start = min(max(index.getOrElse(0) - terms / 2, 0), xi.length - terms - 1)
    val end = start + terms
    val lx = xi.slice(start, end)
    val lf = fi.slice(start, end)
    (0.0 /: (for(i <- 0 until terms) yield L(x, i, lx) * lf(i)))(_+_)
  }
}

class EvenlySpacedLagrangePolynomial(val xmin : Double, 
        val xmax : Double, val fi : Array[Double], val terms : Int) {
  private val xi = (for(i <- 0 until fi.length) yield 2.0 * i / (fi.length - 1.0) - 1.0).toArray
  private final def scale(x : Double) = 2.0 * (x - xmin) / (xmax - xmin) - 1.0
  
  //Need to be careful on the units here. This should be right.
  private final def dL(x : Double, i : Int, lx : Array[Double]) = {
    val elements = for(k <- 0 until terms; if(k != i); c = 1.0 / (lx(i) - lx(k)))
      yield (c /: (for(j <- 0 until terms; if(j != i && j != k)) yield (x - lx(j))/(lx(i) - lx(j))))(_*_)
    (0.0 /: elements)(_+_) / (0.5 * (xmax - xmin))
  }
  
  private final def L(x : Double, i : Int, lx : Array[Double]) = 
    (1.0 /: (for(j <- 0 until terms; if(j != i)) yield (x - lx(j))/(lx(i) - lx(j))))(_*_)
  
  def apply(xs : Double) = {
    val x = scale(xs)
    val index = (1 until xi.length).find(i => (x >= xi(i-1) && x <= xi(i)))
    val start = min(max(index.getOrElse(0) - terms / 2, 0), xi.length - terms - 1)
    val end = start + terms
    val lx = xi.slice(start, end)
    val lf = fi.slice(start, end)
    (0.0 /: (for(i <- 0 until terms) yield L(x, i, lx) * lf(i)))(_+_)
  }
  
  def d(xs : Double) = {
    val x = scale(xs)
    val index = (1 until xi.length).find(i => (x >= xi(i-1) && x <= xi(i)))
    val start = min(max(index.getOrElse(0) - terms / 2, 0), xi.length - terms - 1)
    val end = start + terms
    val lx = xi.slice(start, end)
    val lf = fi.slice(start, end)
    (0.0 /: (for(i <- 0 until terms) yield dL(x, i, lx) * lf(i)))(_+_)
  }
}

class LagrangePolynomial2D(val t : Array[Double], val x : Array[Double], val y : Array[Double], val terms : Int) {
  private val lx = new EvenlySpacedLagrangePolynomial(t.head, t.last, x, terms)
  private val ly = new EvenlySpacedLagrangePolynomial(t.head, t.last, y, terms)
  def apply(xu : Double) = (lx(xu), ly(xu))
  def d(xu : Double) = (lx.d(xu), ly.d(xu))
}

class LagrangePolynomial3D(val t : Array[Double], val x : Array[Double], 
                           val y : Array[Double], val z : Array[Double], val terms : Int) {
//  private val lx = new LagrangePolynomial(t, x, 12)
//  private val ly = new LagrangePolynomial(t, y, 12)
//  private val lz = new LagrangePolynomial(t, z, 12)
  private val lx = new EvenlySpacedLagrangePolynomial(t.head, t.last, x, terms)
  private val ly = new EvenlySpacedLagrangePolynomial(t.head, t.last, y, terms)
  private val lz = new EvenlySpacedLagrangePolynomial(t.head, t.last, z, terms)
  def apply(xu : Double) = (lx(xu), ly(xu), lz(xu))
  def d(xu : Double) = (lx.d(xu), ly.d(xu), lz.d(xu))
}

class CubicSpline(val x : Array[Double], val y : Array[Double]) {
  val n = x.length
  val minimum = x(0)
  val maximum = x(x.length - 1)
//  val s = for(xx <- x) yield scale(xx)
  val d2x = tridiag  
  
  private final def scale(ss : Double) = 2.0 * (ss - minimum) / (maximum - minimum) - 1.0
  private final def unscale(ss : Double) = (ss + 1.0) * 0.5 * (maximum - minimum) + minimum
  
  private final def tridiag = {
    val e = new Array[Double](n-2)
    val f = new Array[Double](n-2)
    val g = new Array[Double](n-2)
    val r = new Array[Double](n-2)
    for(i <- 1 until n-1) {
      if(i != 1) e(i-1) = bd(i, x)
      f(i-1) = 2.0 * cd(i, x)
      if(i != n-1) g(i-1) = fd(i, x)
      r(i-1) = 6.0 * (fd(i, y) / fd(i, x) - bd(i, y) / bd(i, x))
    }
    
    val res = new Array[Double](n)
    Array.copy(Thomas(e, f, g, r), 0, res, 1, n-2)
    res
  }
  
  def bd(i : Int, a : Array[Double]) = (a(i) - a(i - 1))
  def fd(i : Int, a : Array[Double]) = (a(i + 1) - a(i))
  def cd(i : Int, a : Array[Double]) = (a(i + 1) - a(i - 1))
  
  def apply(xu : Double) = {
    //val ss = scale(xu)
    val index = (1 until n).find(i => (xu >= x(i-1) && xu <= x(i)))
    for(i <- index;
        bdx = x(i) - x(i - 1);
        c1 = d2x(i - 1) / 6.0 / bdx;
        c2 = d2x(i) / 6.0 / bdx;
        c3 = y(i - 1) / bdx - d2x(i - 1) * bdx / 6.0;
        c4 = y(i) / bdx - d2x(i) * bdx / 6.0;
        ximxu = (x(i) - xu);
        ximxu2 = ximxu * ximxu;
        ximxu3 = ximxu2 * ximxu;
        xumxi = xu - x(i - 1);
        xumxi2 = xumxi * xumxi;
        xumxi3 = xumxi2 * xumxi;
        t1 = c1 * ximxu3;
        t2 = c2 * xumxi3;
        t3 = c3 * ximxu;
        t4 = c4 * xumxi;
        yu = t1 + t2 + t3 + t4;
        dt1 = -3.0 * c1 * ximxu2;
        dt2 = 3.0 * c2 * xumxi2;
        dt3 = -c3;
        dt4 = c4;
        dy = dt1 + dt2 + dt3 + dt4;
        ddt1 = 6.0 * c1 * ximxu;
        ddt2 = 6.0 * c2 * xumxi;
        d2y = ddt1 + ddt2)
       yield (yu, dy, d2y)
  }
}

class CubicSpline2D(val t : Array[Double], val x : Array[Double], val y : Array[Double]) {
  private val cx = new CubicSpline(t, x)
  private val cy = new CubicSpline(t, y)
  def apply(xu : Double) = for(pvax <- cx(xu); pvay <- cy(xu)) 
    yield ((pvax._1, pvay._1), (pvax._2, pvay._2), (pvax._3, pvay._3))
}

class CubicSpline3D(val t : Array[Double], val x : Array[Double], 
                    val y : Array[Double], val z : Array[Double]) {
  private val cx = new CubicSpline(t, x)
  private val cy = new CubicSpline(t, y)
  private val cz = new CubicSpline(t, z)
  def apply(xu : Double) = for(pvax <- cx(xu); pvay <- cy(xu); pvaz <- cz(xu)) 
    yield ((pvax._1, pvay._1, pvaz._1), (pvax._2, pvay._2, pvaz._2), (pvax._3, pvay._3, pvaz._3))
}

object Thomas {
  def apply(e : Array[Double], f : Array[Double], g : Array[Double], r : Array[Double], n : Int) : Array[Double] = {
    decompose(e, f, g, n)
    forwardSubstitite(e, r, n)
    backwardSubstitite(f, g, r, n)
  }
  
  def apply(e : Array[Double], f : Array[Double], g : Array[Double], r : Array[Double]) : Array[Double] =
      this(e, f, g, r, f.length)
  
  private def decompose(e : Array[Double], f : Array[Double], g : Array[Double], n : Int) = 
    for(k <- 1 until n) {
      e(k) /= f(k - 1)
      f(k) -= e(k) * g(k - 1)
  }
  
  private def forwardSubstitite(e : Array[Double], r : Array[Double], n : Int) = 
    for(k <- 1 until n) r(k) -= e(k) * r(k - 1)
    
  private def backwardSubstitite(f : Array[Double], g : Array[Double], r : Array[Double], n : Int) = {
    val x = new Array[Double](n)
    x(n - 1) = r(n - 1) / f(n - 1)
    for(k <- (n - 2) to 0 by -1) x(k) = (r(k) - g(k) * x(k+1)) / f(k)
    x
  }
}