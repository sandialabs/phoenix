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

package gov.sandia.phoenix.starcat.hipparcos

import gov.sandia.phoenix.geometry._
import gov.sandia.phoenix.time._

/**
 * [[http://tdc-www.harvard.edu/software/catalogs/catalogsb.html Binary File Format]]
 *
 * ===Usage (Scala)===
 * {{{
 import gov.sandia.phoenix.starcat.hipparcos._
 import gov.sandia.phoenix.time._
 import gov.sandia.phoenix.core.util.MathEX._
 import gov.sandia.phoenix.core._
 import gov.sandia.phoenix.geometry._

 val cat = HipparcosCatalog.default
 //http://en.wikipedia.org/wiki/Theta_Persei
 val star = cat.entries find { _.catalogNumber == 12777 } get
 val t = TimeBuilder(2028, 11, 13) plusDays 0.19
 val radec = star.radec(t)
 val ra = radec.rightAscension
 val dec = radec.declination
 DD2DMS(ra)
 DD2DMS(dec)
 val fk5 = t.fk5

 //Meeus Example
 
 //This reproduces example 20.b of Meeus. There are small differences, but I
 //use the zeta, theta, z calcs from Vallado. The differences are tiny.
 val a = (2 + (44 + 11.986 / 60) / 60.0) / 24.0 * 360
 val d = 49 + (13.0 + 42.48 / 60) / 60
 val da = 0.03425 / 240
 val dd = -0.0895 / 3600
 val dt = t.toJulianCenturyJ2000 * 100
 val radechc = new RaDec(RightAscension.fromDegrees(a), Declination.fromDegrees(d))
 val radechca = new RaDec(RightAscension.fromDegrees(radechc.rightAscension.degrees + da * dt), Declination.fromDegrees(radechc.declination.decimalDegrees + dd * dt))
 fk5.equatorialReduction(radechca)
 * }}}
 */
case class HipparcosCatalogEntry(header : HipparcosCatalogHeader, catalogNumber : Int, meanRaDec : RaDec, ISP : String,
                                 mags : Array[Float], pmRA : Float, pmDec : Float) {

  //Right now I only deal with J2000 based catalogs.
  require(header.NMAG < 0)

  def radec(t : JD) = {
    val dt = t.toJulianCenturyJ2000 * 100
    val ra = meanRaDec.rightAscension.degrees + pmRA * dt
    val dec = meanRaDec.declination.decimalDegrees + pmDec * dt
    t.fk5.equatorialReduction(new RaDec(RightAscension.fromDegrees(ra), Declination.fromDegrees(dec)))
  }

  def Bt = mags(0)
  def Vt = mags(1)
  def parallax = mags(2)
  def parallaxError = mags(3)
  lazy val visualMagnitude = if(Vt != 0.0) Vt -0.090f * (Bt - Vt) else Bt

  def temperature = 4600 * (1.0 / (0.92 * (Bt - Vt) + 1.7) + 1.0 / (0.92 * (Bt - Vt) + 0.62))
  
  def pretty = {
    val coords = if(header.NMAG < 0) "J2000" else "B1950"

    "Catalog Number: " + catalogNumber + "\u00B0\n" +
    coords + " R.A. : " + meanRaDec.rightAscension + "\u00B0\n" +
    coords + " Dec: " + meanRaDec.declination + "\n" +
    "Spectral Type: " + ISP + "\n" +
    "V Magnitude: " + mags.mkString(", ") + "\n" +
    "R.A. Proper Motion (\u00B0/yr): " + pmRA + "\n" +
    "Dec Proper Motion (\u00B0/yr): " + pmDec + "\n"
  }
}