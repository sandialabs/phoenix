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

package gov.sandia.phoenix.elements.gps.alm

import java.util.Date

import java.util.logging.Logger

object DefaultAlmanac {
  val logger = Logger.getLogger(getClass.getName)
}

class DefaultAlmanac (
  val PRN : Int, val SOH : Byte,
  val e : Double, val t_oa : Float,
  val i_0 : Double,
  val OMEGADOT : Double, val sqrtA : Double,
  val OMEGA_0 : Double, val omega : Double,
  val M_0 : Double, val a_f0 : Float,
  val a_f1 : Float, val weekNumber : Int) extends Almanac {
  DefaultAlmanac.logger.fine("Almanac created with the following parameters:\n" + this.toString)

  override def toString =
  {
    val buff = new StringBuffer
    buff.append("PRN = " + PRN + "\n")
    buff.append("e0 = " + e + "\n")
    buff.append("time of applicability = " + t_oa + "\n")
    buff.append("i_0 = " + i_0 + "\n")
    buff.append("didt = " + rateOfInclination + "\n")
    buff.append("OMEGADOT = " + OMEGADOT + "\n")
    buff.append("sqrtA = " + sqrtA + "\n")
    buff.append("OMEGA_0 = " + OMEGA_0 + "\n")
    buff.append("omega = " + omega + "\n")
    buff.append("M_0 = " + M_0 + "\n")
    buff.append("a_f0 = " + a_f0 + "\n")
    buff.append("a_f1 = " + a_f1 + "\n")
    buff.append("weeknumber = " + weekNumber + "\n")
    buff.toString
  }

  def argumentOfPerigee=omega
  def af0=a_f0
  def af1=a_f1
  def inclination=i_0
  def eccentricity=e
  def epochDate:Date=null
  def longitudeOfAscendingNode=OMEGA_0
  def meanAnomaly=M_0
  def rateOfRightAscension=OMEGADOT
  def timeOfApplicability=t_oa
  def squareRootOfSemiMajorAxis=sqrtA
}
