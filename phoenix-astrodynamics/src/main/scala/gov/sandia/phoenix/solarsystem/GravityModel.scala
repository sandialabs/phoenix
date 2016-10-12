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

package gov.sandia.phoenix.solarsystem

import gov.sandia.phoenix.constants._
import gov.sandia.phoenix.geometry._

import scala.math._

/**
 *
 *===Usage (Scala)==={{{
 import gov.sandia.phoenix.core.celestial._
 import gov.sandia.phoenix.constants._
 import gov.sandia.phoenix.geometry._
 import scala.io._
 import scala.math._

 println(EGM96GravityModel(new ECEF(WGS84.R_EQ_M, 0, 0), 20, 20))
 println(WGS84_EGM96GravityModel(new ECEF(WGS84.R_EQ_M, 0, 0), 20, 20))
 println(WGS84GravityModel(new ECEF(WGS84.R_EQ_M, 0, 0), 20, 20))
 println(WGS84GravityModel(new ECEF(0, WGS84.R_EQ_M, 0), 20, 20))
 println(WGS84GravityModel(new ECEF(0, 0, WGS84.R_EQ_M), 20, 20))
 println(JGM3GravityModel(new ECEF(0, 0, WGS84.R_EQ_M), 20, 20))
 *}}}
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
class GravityModel(val GM : Double, val R : Double, val g : GravitationalCoefficients) {

  private val GMR2 = GM / (R * R)
  
  def apply(r : Vector3) : Vector3 = this(r, 20, 20)
  
  def apply(r : Vector3, ndesired : Int, mdesired : Int) : Vector3 = {
    val r2 = r * r
    val rho   =  R*R / r2
    val v = r * (R / r2)
    val V = Array.ofDim[Double](ndesired, mdesired)
    val W = Array.ofDim[Double](ndesired, mdesired)

    def VW(n : Int, m : Int) = (n, m) match {
      case _ if n == 0 && m == 0 =>
        V(n)(m) = R / sqrt(r2)
        W(n)(m) = 0.0
      case _ if n == m =>
        val vm = V(m - 1)(m - 1)
        val wm = W(m - 1)(m - 1)
        V(n)(m) = (2.0 * m - 1) * (v.x * vm - v.y * wm)
        W(n)(m) = (2.0 * m - 1) * (v.x * wm + v.y * vm)
      case _ =>
        val a = v.z * (2.0 * n - 1) / (n - m)
        val b = rho * (n + m - 1.0) / (n - m)
        V(n)(m) = a * V(n - 1)(m) - (if(b > 0.0) b * V(n - 2)(m) else 0)
        W(n)(m) = a * W(n - 1)(m) - (if(b > 0.0) b * W(n - 2)(m) else 0)
    }

    for(n <- 0 until V.length; m <- 0 to n) VW(n, m)
  
    def acc(f : (Int, Int) => Double) = (0.0 /: (for(m <- 0 until mdesired - 1; n <- m until ndesired - 1) yield f(n, m)))(_+_)
    def ax = acc(axnm)
    def ay = acc(aynm)
    def az = acc(aznm)

    def axnm(n : Int, m : Int) = m match {
      case _ if m < 0 => 0.0
      case 0 => -g.C(n,0) * V(n+1)(1)
      case _ if m > 0 => 0.5 * (-g.C(n,m) * V(n+1)(m+1) - g.S(n,m) * W(n+1)(m+1) + (n - m + 1)*(n - m + 2)*(g.C(n,m) * V(n+1)(m-1) + g.S(n,m) * W(n+1)(m-1)))
    }

    def aynm(n : Int, m : Int) = m match {
      case _ if m < 0 => 0.0
      case 0 => -g.C(n,0) * W(n+1)(1)
      case _ if m > 0 => 0.5 * (-g.C(n,m) * W(n+1)(m+1) + g.S(n,m) * V(n+1)(m+1) + (n - m + 1)*(n - m + 2)*(-g.C(n,m) * W(n+1)(m-1) + g.S(n,m) * V(n+1)(m-1)))
    }
  
    def aznm(n : Int, m : Int) = (n - m + 1)*(-g.C(n,m) * V(n+1)(m) - g.S(n,m) * W(n+1)(m))

    Vector3(ax * GMR2, ay * GMR2, az * GMR2)
  }
}

object EGM96GravityModel extends GravityModel(3986004.415E8, 6378136.3, EGM96GravityCoefficients)
object WGS84_EGM96GravityModel extends GravityModel(WGS84.GM, WGS84.R_EQ_M, EGM96GravityCoefficients)
object WGS84GravityModel extends GravityModel(WGS84.GM, WGS84.R_EQ_M, WGS84GravityCoefficients)
object JGM3GravityModel extends GravityModel(398600.44150E+09, 6378136.30, JGM3GravityCoefficients)