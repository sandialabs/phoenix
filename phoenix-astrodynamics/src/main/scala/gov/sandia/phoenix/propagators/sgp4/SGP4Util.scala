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

object SGP4Util {
  def mod2pi(x : Double) = x % (2.0 * Pi)

  //Values used for dsinit in ElementSetRecord
  val q22 = 1.7891679e-6
  val q31 = 2.1460748e-6
  val q33 = 2.2123015e-7
  val root22 = 1.7891679e-6
  val root44 = 7.3636953e-9
  val root54 = 2.1765803e-9
  val rptim = 4.37526908801129966e-3; // this equates to 7.29211514668855e-5 rad/sec
  val root32 = 3.7393792e-7
  val root52 = 1.1428639e-7

  def kozaiToBrouwer(n0 : Double, i0 : Double, e0 : Double, c : GravityConstants) = {
    val a1 = pow(c.xke / n0, 2.0 / 3.0)
    val d = 0.75 * c.j2 * (3.0 * cos(i0) * cos(i0) - 1.0) / pow(1.0 - e0 * e0, 1.5)
    val d1 = d / (a1 * a1)
    val a2 = a1 * (1.0 - (1.0 / 3.0 + (1.0 + 134.0 / 81.0 * d1) * d1) * d1)
    val d0 = d / (a2 * a2)
    val n = n0 / (1.0 + d0)
    (n, pow(c.xke / n, 2.0 / 3.0))
  }
}