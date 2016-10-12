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

import gov.sandia.phoenix.propagators.sgp4._
import gov.sandia.phoenix.time.JD

import scala.math._

trait MeanElements {
  def I0: Double
  def Ω0: Double
  def e0: Double
  def ω0: Double
  def meanAnomaly: Double
  def meanMotion: Double
  def epoch: JD
  def bStarDrag: Double
  def gc: GravityConstants

  val jdepoch = epoch.doubleValue
  val gmst = epoch.toGMST.doubleValue.toRadians
  val θ = cos(I0)
  val β0 = sqrt(1.0 - e0 * e0)

  val (no, ao) = SGP4Util.kozaiToBrouwer(meanMotion, I0, e0, gc)
  val rp = ao * (1.0 - e0)

  var error = ""
  if(rp < 1.0) {
    error = "epoch elements are sub-orbital"
    logger.severe(error)
  }

  //Note that the following should only be created conditionally for deep space propagation.
  val l = LSCoefficients(LunarConstants(jdepoch), Ω0, I0, ω0, e0, no)
  val s = LSCoefficients(SolarConstants(jdepoch), Ω0, I0, ω0, e0, no)
  val dedt = s.de + l.de
  val didt = s.dI + l.dI
  val dmdt = s.dM + l.dM
  val dnodt = s.dΩ + l.dΩ
  val domdt = s.dω + l.dω
  //End stuff we don't always need

  //Appendix B
  val perigee = (rp - 1.0) * gc.radiusearthkm
  
  val (qms4, sfour) = if(perigee < 156.0) {
    val x = if(perigee < 98.0) 20.0 else perigee - 78.0
    (pow((120.0 - x) / gc.radiusearthkm, 4.0), x / gc.radiusearthkm + 1.0)
  } else (pow((120.0 - 78.0) / gc.radiusearthkm, 4), 78.0 / gc.radiusearthkm + 1.0)

  val ξ = 1.0 / (ao - sfour)
  val η = ao * e0 * ξ
  
  //Useful intermediate variables
  val k2oa2 = gc.j2 / (ao * ao * 2.0)
  val aβ2 = ao * β0 * β0
  val a2β4 = aβ2 * aβ2
  val η2 = η * η
  val θ2 = θ * θ
  val Cpre = qms4 * pow(ξ, 4.0) / pow(abs(1.0 - η2), 3.5)

  val C2 = Cpre * no * (ao * (1.0 + (4.0 * e0 + (1.5 + e0 * η) * η) * η) +
      (0.75 * gc.j2 * ξ / abs(1.0 - η2)) * (1.5 * θ2 - 0.5) * (8.0 + (24.0 + 3.0 * η2) * η2))

  val C1 = bStarDrag * C2

  val C3 = if(e0 > 1.0e-4) -2.0 * qms4 * pow(ξ, 5.0) * gc.j3oj2 * no * sin(I0) / e0 else 0.0

  val C4 = 2.0 * Cpre * no * aβ2 *
  (0.5 * e0 + (2.0 + (2.0 * e0 + 0.5 * η) * η) * η -
    gc.j2 * ξ / (ao * abs(1.0 - η * η)) *
      (3.0 * (1.0 - 3.0 * θ2) *
        (1.0 - (2.0 * e0 - (1.5 - 0.5 * e0 * η) * η) * η) +
        0.75 * (1.0 - θ2) * ((-e0 + (2.0 - e0 * η) * η) * η) * cos(2.0 * ω0)))

  val C5 = 2.0 * Cpre * aβ2 * (1.0 + 2.75 * (η * η + e0 * η) + e0 * η * η * η)

  //Appendix B.2: Initialization for Secular Effects of Earth Zonal Harmonics
  //These don't all quite jive with Hoots. Why?
  val dM = 3.0 * no * ((2.0 / 3.0 * (β0 * β0 * β0)) + (k2oa2 * (3.0 * θ2 - 1.0)) +
    (k2oa2 * k2oa2) * (13.0 - (78.0 - 137.0 * θ2) * θ2) / (8.0 * pow(β0, 4))) / (2.0 * β0 * β0 * β0)

  val dω = -3.0 * no * (16.0 * gc.j2 * (1.0 - 5.0 * θ2) -
    (gc.j2 * gc.j2 * (7.0 - (114.0 - 395.0 * θ2) * θ2) -
      10.0 * gc.j4 * (3.0 - (36.0 - 49.0 * θ2) * θ2)) / a2β4) / (64.0 * a2β4)

  val dΩ = -3.0 * θ * no * (8.0 * gc.j2 -
    (2.0 * gc.j2 * gc.j2 * (4.0 - 19.0 * θ2) -
      5.0 * gc.j4 * (3.0 - 7.0 * θ2)) / a2β4) / (a2β4 * 16.0)

  //(rp - 1) * gc.radiusearthkm < 220.0

  val isimp = if(rp < (220.0 / gc.radiusearthkm + 1.0) || (2 * Pi / no) >= 225.0) 1 else 0

  /* --------------- deep space initialization ------------- */
  val (method, resonanceTerms) = if((2 * Pi / no) >= 225.0){
    //This is where the lunar, solar, etc. terms should be created only once.
    (DeepSpace, ResonanceTermsSwitcher.getType(this))
  } else (NearSpace, NoResonance)

  /* ----------- set variables if not deep space ----------- */
  val (d2, d3, d4, t3cof, t4cof, t5cof) = if(isimp != 1) {
    val D2 = 4.0 * ao * ξ * C1 * C1
    val D3 = (17.0 * ao + sfour) * D2 * ξ * C1 / 3.0
    val D4 = 0.5 * D2 * ξ * C1 / 3.0 * ao * ξ * (221.0 * ao + 31.0 * sfour) * C1
    val t3cof = D2 + 2.0 * C1 * C1
    val t4cof = 0.25 * (3.0 * D3 + C1 * (12.0 * D2 + 10.0 * C1 * C1))
    val t5cof = 0.2 * (3.0 * D4 + 12.0 * C1 * D3 + 6.0 * D2 * D2 + 15.0 * C1 * C1 * (2.0 * D2 + C1 * C1))
    (D2, D3, D4, t3cof, t4cof, t5cof)
  } else (0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
  //TODO: Fix this. Note that caps in pattern matching fails.
  val D2 = d2
  val D3 = d3
  val D4 = d4
  
  def calcPeriodics(tsince : Double) = {
    val sp = s.periodics(tsince)
    val lp = l.periodics(tsince)
    Periodics(sp(0) + lp(0), sp(1) + lp(1), sp(2) + lp(2), sp(3) + lp(3), sp(4) + lp(4))
  }

  /**
   * Coversion equations from Appendex B.B.1
   * Secular Update for Earth Zonal Gravity and Partial Atmospheric Drag Effects
   */
  def update(args : DeepSpaceArgs, Δt: Double) = if(isimp != 1) {
    val δω = bStarDrag * C3 * cos(ω0) * Δt
    val c = if(e0 > 1.0e-4) -2.0/3.0 * qms4 * pow(ξ, 4.0) * bStarDrag / (e0 * η) else 0.0
    val δM = c * (pow(1.0 + η * cos(args.M0), 3) - pow(1.0 + η * cos(meanAnomaly), 3))
    val δ = δω + δM
    new DeepSpaceArgs(args.e0, args.ω0 - δ, args.I0, args.M0 + δ, args.Ω0, args.n0)
  } else args

  def LuniSolarUpdate(args : DeepSpaceArgs, Δt: Double) =
    if(method == DeepSpace) args.resonanceUpdate(this, Δt) else args
}
