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

package gov.sandia.phoenix.propagators.sgp4

import scala.math._

case class LunarConstants(jdepoch : Double) extends LSConstants {
  val Ωme = SGP4Util.mod2pi(4.5236020 - 9.2422029e-4 * ((jdepoch - 2433281.5) + 18261.5))
  val cIxl = 0.91375164 - 0.03568096 * cos(Ωme)
  val sIxl = sqrt(1.0 - cIxl * cIxl)
  val sΩ = 0.089683511 * sin(Ωme) / sIxl
  val cΩ = sqrt(1.0 - sΩ * sΩ)
  val zy = cΩ * cos(Ωme) + 0.91744867 * sΩ * sin(Ωme)
  val ω = 5.8351514 + 0.0019443680 * ((jdepoch - 2433281.5) + 18261.5) + atan2(0.39785416 * sin(Ωme) / sIxl, zy) - Ωme
  val cω = cos(ω)
  val sω = sin(ω)
  val I = 5.145396374
  val cI = cIxl
  val sI = sIxl
  //    val sI = sin(I.toRadians)
  //    val cI = cos(I.toRadians)
  val eccentricity = 0.05490
  //Lunar mean motion, in radians per minute
  //Note that Hoots uses 1.583521770E-4.
  val meanMotion = 1.5835218e-4
  val C = 4.7968065e-7 //Lunar perturbation coefficient (Radians per minute)

  //Used in Appendix A.E.
  //See Appendix B.B.5 Update for Long-Period Periodic Effects of Lunar and Solar Gravity (MX =MOX + nX * dt)
  def M(tsince : Double) = SGP4Util.mod2pi(-1.1151842 + 0.228027132 * ((jdepoch - 2433281.5) + 18261.5)) + meanMotion * tsince
}
