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

import java.io.InputStream
import java.util.Scanner

/**
 * @see http://blogs.agi.com/navigationAccuracy/?p=113
 * @see http://www.navcen.uscg.gov/?pageName=gpsAlmanacs
 */
object YUMAAlmanac {
  private val ptrn = "\\**\\s*Week\\s*(\\d*)\\s*almanac for PRN-(\\d*)\\s*\\**"
  private val r = ptrn.r
  
  def load(is : InputStream) = read(new Scanner(is))
  
  private def read(s : Scanner, res : List[Almanac] = Nil) : List[Almanac] = if(s.hasNextLine) {
    s.nextLine match {
      case r(week, prn) => read(s, readAlmanac(s) :: res)
      case _ => read(s, res)
    }
  } else res
  
  private def readAlmanac(scanner : Scanner) = {
    val prn = scanner.nextLine.split(":")(1).trim.toInt
    val stateOfHealth = scanner.nextLine.split(":")(1).trim.toInt
    val eccentricity = scanner.nextLine.split(":")(1).trim.toDouble
    val toa = scanner.nextLine.split(":")(1).trim.toDouble
    val inclination = scanner.nextLine.split(":")(1).trim.toDouble
    val rateOfRaan = scanner.nextLine.split(":")(1).trim.toDouble
    val sqrtA = scanner.nextLine.split(":")(1).trim.toDouble
    val raan = scanner.nextLine.split(":")(1).trim.toDouble
    val perigee = scanner.nextLine.split(":")(1).trim.toDouble
    val meanAnomaly = scanner.nextLine.split(":")(1).trim.toDouble
    val af0 = scanner.nextLine.split(":")(1).trim.toFloat
    val af1 = scanner.nextLine.split(":")(1).trim.toFloat
    val weekNum = scanner.nextLine.split(":")(1).trim.toInt
        
    val builder = new DefaultAlmanacBuilder
    builder.argumentOfPerigee(perigee)
    builder.squareRootOfSemiMajorAxis(sqrtA)
    builder.eccentricity(eccentricity)
    builder.inclination(inclination)
    builder.longitudeOfAscendingNode(raan)
    builder.rateOfRightAscension(rateOfRaan)
    builder.PRN(prn)
    builder.meanAnomaly(meanAnomaly)
    builder.af0(af0)
    builder.af1(af1)
    builder.weekNumber(weekNum)
    builder.timeOfApplicability(toa.floatValue)
    builder.stateOfHealth(stateOfHealth.toByte)
        
    builder.build
  }
}
