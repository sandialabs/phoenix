package gov.sandia.phoenix.propagators

import java.util.logging.Logger
import scala.math._

/**
 * SGP4 : Simplified General Perturbations 4 is by far the most common method for propagating satellite locations beyond
 * simple Kepler Element/Two-body propagation. The SGP4 reference implementation can be read about in the ancient paper
 * "SPACETRACK REPORT NO. 3," (STR3) found here https://celestrak.com/NORAD/documentation/spacetrk.pdf. This paper
 * includes all equations, both typeset as well as an implementation in FORTAN.
 *
 * A more recent implementation of this algorithm can be found accompanying the paper "Revisiting Space
 * Track Report #3," (RSTR3) found here: https://celestrak.com/publications/AIAA/2006-6753/.
 *
 * A few observations on the above:
 *
 * The publicly available RSTR3 code is not particularly well commented and has hard to read variable names and code blocks. This
 * is likely due to its resemblance to the original FORTRAN implementation, which also suffers from these problems. One
 * of the biggest challenges I have faced in working with SGP4 is knowing what equations came from what source or
 * formula in the original paper.
 *
 * Given the above, this implementation of SGP4 is based on the code from RSTR3, ported to Scala, but also makes an
 * attempt to reconcile all equations with equations from STR3 as well as the paper
 * "History of Analytical Orbit Modeling in the U.S. Space Surveillance System," by Felix R. Hoots, Paul W. Schumacher
 * Jr., and Robert A. Glover. A careful study of RSTR3 has been done and, where possible, equations and values have been
 * extracted and renamed to more fully match the equations found in STR3 and Hoots.
 *
 * Some difficulties have arisen, such as finding equations for updating lunar and solar gravity long periodic effects
 * since those equations are referenced in the 1961 edition of the
 * "Explanatory Supplement to the Astronomical Ephemeris and the American Ephemeris and Nautical Almanac." The most
 * recent version of this text, "The Explanatory Supplement to the Astronomical Almanac," was published in 2012 varies
 * greatly from the original and pagination and other updates have made a simple reference to the source equations
 * difficult. Note that when writing this I found this archive
 * "https://archive.org/stream/astronomicalalmanac1961/131221-explanatory-supplement-1961_djvu.txt" which might be
 * useful in locating original source equations.
 *
 * The ultimate goal of this implementation are:
 * 1. Compliance with existing versions.
 * 2. Readability - This is a work in progress, but the goal is the have all equations and constants match their
 * descriptions from the original works.
 * 3. Simplicity - This is an extremely simple API to use, having methods for construction, consumption, and analysis of
 * TLEs as well as a simple API for actual SGP4 propagation.
 */
package object sgp4 {
  val logger = Logger.getLogger(getClass.getName)
  //http://en.wikipedia.org/wiki/Axial_tilt
  //"Obiquity of the Ecliptic"
  val ε = 23.4441 //Obliquity of the ecliptic

  val fasx2 = 0.13130908
  val fasx4 = 2.8843198
  val fasx6 = 0.37448087
  val G22 = 5.7686396
  val G32 = 0.95240898
  val G44 = 1.8014998
  val G52 = 1.0508330
  val G54 = 4.4108898
  val rptim = 4.37526908801129966e-3
  val stepp = 720.0
  val step2 = 259200.0

  def D(l : Int, m : Int, p : Int, q : Int, a0 : Double, n0 : Double, e0 : Double, i0 : Double) = {
    val a = 3.0 * n0 * n0 / pow(a0, l)
    val cs = CS(l, m)
    val f = F(l, m, p, i0)
    val g = G(l, p, q, e0)
    a * cs * f * g * m / 2
  }

  def CS(l : Int, m : Int) = (l, m) match {
    case (2, 2) => 1.7891679E-6
    case (3, 2) => 3.7393792E-7
    case (4, 4) => 7.3636953E-9
    case (5, 2) => 1.1428639E-7
    case (5, 4) => 2.1765803E-9
    case _ => 0.0
  }

  def F(l : Int, m : Int, p : Int, i0 : Double) : Double = {
    val si = sin(i0)
    val ci = cos(i0)
    val cp1 = ci + 1.0
    val s2 = si * si
    val s3 = s2 * si
    val s4 = s2 * s2
    (l, m, p) match {
      case (2, 2, 0) => 0.75 * cp1 * cp1
      case (2, 2, 1) => 1.5 * s2
      case (3, 2, 1) => 1.875 * si * (1.0 - (2.0 + 3.0 * ci) * ci)
      case (3, 2, 2) => -1.875 * si * (1.0 + (2.0 - 3.0 * ci) * ci)
      case (4, 4, 1) => //35.0 * s2 * F(2, 2, 0, Ix)
        //The recursive call does not give exactly the same result as the direct call. Hmmm?
        26.25 * s2 * cp1 * cp1 //xxx
      case (4, 4, 2) => 39.375 * s4
      case (5, 2, 2) => 9.84375 * (s3 * (1.0 - (2.0 + 5.0 * ci) * ci) + si * (-2.0 + (4.0 + 6.0 * ci) * ci) / 3.0)
      case (5, 2, 3) => 6.5625 * si * (1.0 + (2.0 - 3.0 * ci) * ci - 1.5 * si * si * (1.0 + (2.0 - 5.0 * ci) * ci))
      case (5, 4, 2) => 29.53125 * si * (2.0 - (8.0 - (-12.0 + (8.0 + 10.0 * ci) * ci) * ci) * ci)
      case (5, 4, 3) => 29.53125 * si * (-2.0 - (8.0 - (12.0 + (8.0 - 10.0 * ci) * ci) * ci) * ci)
      case _ =>
        logger.warning("Unknown F triplet: " + l + ", " + m + ", " + p)
        0.0
    }
  }

  def G(l : Int, p : Int, q : Int, e0 : Double) = (l, p, q) match {
    case (2, 0, 1) => -0.306 - 0.44 * (e0 - 0.64)
    case (2, 1, 1) => if(e0 <= 0.65) 3.616 + (-13.2470 + 16.2900 * e0) * e0 else
      -72.099 + (331.819 + (-508.738 + 266.724 * e0) * e0) * e0
    case (3, 1, 0) => if(e0 <= 0.65) -19.302 + (117.3900 + (-228.4190 + 156.5910 * e0) * e0) * e0 else
      -346.844 + (1582.851 +(-2415.925 + 1246.113 * e0) * e0) * e0
    case (3, 2, 2) => if(e0 <= 0.65) -18.9068 + (109.7927 + (-214.6334 + 146.5816 * e0) * e0) * e0 else
      -342.585 + (1554.908 + (-2366.899 + 1215.972 * e0) * e0) * e0
    case (4, 1, 0) => if(e0 <= 0.65) -41.122 + (242.6940 + (-471.0940 + 313.9530 * e0) * e0) * e0 else
      -1052.797 + (4758.686 + (-7193.992 + 3651.957 * e0) * e0) * e0 //xx
    case (4, 2, 2) => if(e0 <= 0.65) -146.407 + (841.8800 + (-1629.014 + 1083.4350 * e0) * e0) * e0 else
      -3581.690 + (16178.110 + (-24462.770 + 12422.520 * e0) * e0) * e0
    case (5, 2, 0) => if(e0 <= 0.65) -532.114 + (3017.977 + (-5740.032 + 3708.2760 * e0) * e0) * e0 else
    if(e0 > 0.715) -5149.66 + (29936.92 + (-54087.36 + 31324.56 * e0) * e0) * e0 else
      1464.74 + (-4664.75 + 3763.64 * e0) * e0
    case (5, 2, 1) => if(e0 <= 0.7) -822.71072 + (4568.6173 + (-8491.4146 + 5337.524 * e0) * e0) * e0 else
      -51752.104 + (218913.95 + (-309468.16 + 146349.42 * e0) * e0) * e0
    case (5, 3, 2) => if(e0 <= 0.7) -853.66600 + (4690.2500 + (-8624.7700 + 5341.4 * e0) * e0) * e0 else
      -40023.880 + (170470.89 + (-242699.48 + 115605.82 * e0) * e0) * e0
    case (5, 3, 3) => if(e0 <= 0.7) -919.22770 + (4988.6100 + (-9064.7700 + 5542.21 * e0) * e0) * e0 else
      -37995.780 + (161616.52 + (-229838.20 + 109377.94 * e0) * e0) * e0
    case _ =>
      logger.warning("Unknown G triplet: " + l + ", " + p + ", " + q)
      0.0
  }
}
