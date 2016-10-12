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

trait LSConstants {
  def ω : Double
  def sω : Double
  def cω : Double
  def I : Double
  def sI : Double
  def cI : Double
  def sΩ : Double
  def cΩ : Double
  def eccentricity : Double
  def meanMotion : Double
  def C : Double
  def M(tsince : Double) : Double
  //Approximation for true anomaly
  def fx(tsince : Double) = {
    //The true anomaly of the perturbing body is approximated by fX = MX + 2eX sin MX (See Appendix A.E)
    val Mx = M(tsince)
    Mx + 2.0 * eccentricity * sin(Mx)
  }
}
