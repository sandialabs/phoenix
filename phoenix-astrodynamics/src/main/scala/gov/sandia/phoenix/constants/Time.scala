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

package gov.sandia.phoenix.constants

/**
 * Time related constants
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
object Time {
  val MS_PER_SEC = 1000L
  val SEC_PER_MIN = 60L
  val MIN_PER_HR = 60L
  val HR_PER_DAY = 24L
  val DAY_PER_WEEK = 7L
  val GPS_ROLLOVER_WEEKS = 1024L
  val HR_PER_WEEK = HR_PER_DAY * DAY_PER_WEEK
  val MIN_PER_WEEK = MIN_PER_HR * HR_PER_WEEK
  val SEC_PER_WEEK = SEC_PER_MIN * MIN_PER_WEEK
  val MS_PER_WEEK = MS_PER_SEC * SEC_PER_WEEK
  val MIN_PER_DAY = MIN_PER_HR * HR_PER_DAY
  val SEC_PER_DAY = SEC_PER_MIN * MIN_PER_DAY
  val MS_PER_DAY = MS_PER_SEC * SEC_PER_DAY
  val SEC_PER_HR = SEC_PER_MIN * MIN_PER_HR
  val MS_PER_HR = MS_PER_SEC * SEC_PER_HR
  val MS_PER_MIN = MS_PER_SEC * SEC_PER_MIN

  val MS_PER_SEC_BD = new java.math.BigDecimal(MS_PER_SEC)
  val SEC_PER_MIN_BD = new java.math.BigDecimal(SEC_PER_MIN)
  val MIN_PER_HR_BD = new java.math.BigDecimal(MIN_PER_HR)
  val HR_PER_DAY_BD = new java.math.BigDecimal(HR_PER_DAY)
  val DAY_PER_WEEK_BD = new java.math.BigDecimal(DAY_PER_WEEK)
  val GPS_ROLLOVER_WEEKS_BD = new java.math.BigDecimal(GPS_ROLLOVER_WEEKS)
  val HR_PER_WEEK_BD = new java.math.BigDecimal(HR_PER_WEEK)
  val MIN_PER_WEEK_BD = new java.math.BigDecimal(MIN_PER_WEEK)
  val SEC_PER_WEEK_BD = new java.math.BigDecimal(SEC_PER_WEEK)
  val MS_PER_WEEK_BD = new java.math.BigDecimal(MS_PER_WEEK)
  val MIN_PER_DAY_BD = new java.math.BigDecimal(MIN_PER_DAY)
  val SEC_PER_DAY_BD = new java.math.BigDecimal(SEC_PER_DAY)
  val MS_PER_DAY_BD = new java.math.BigDecimal(MS_PER_DAY)
  val SEC_PER_HR_BD = new java.math.BigDecimal(SEC_PER_HR)
  val MS_PER_HR_BD = new java.math.BigDecimal(MS_PER_HR)
  val MS_PER_MIN_BD = new java.math.BigDecimal(MS_PER_MIN)

  val EARTH_MEAN_SIDEREAL_DAY_MS = 86164091
  val EARTH_MEAN_SIDEREAL_DAY_SEC = EARTH_MEAN_SIDEREAL_DAY_MS.toDouble / MS_PER_SEC
  val EARTH_MEAN_SIDEREAL_DAY = EARTH_MEAN_SIDEREAL_DAY_MS.toDouble / MS_PER_DAY
  val EARTH_MEAN_ROTATION_RATE_RADS_PER_SEC = 2.0 * scala.math.Pi / EARTH_MEAN_SIDEREAL_DAY_SEC
}
