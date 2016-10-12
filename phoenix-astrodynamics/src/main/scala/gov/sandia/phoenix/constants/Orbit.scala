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

import scala.math._

/**
 * Constants relating to satellite orbits.
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
object Orbit {
  val GEO_RADIUS_M = {
    //Was 42164 km, but not accurate enough
    val lhs = 0.5 * Time.EARTH_MEAN_SIDEREAL_DAY_SEC / Pi
    cbrt(lhs * lhs * WGS84.GM)
  }
  
  //was 3.075 km/s
  val GEO_VELOCITY_M_PER_SEC = sqrt(WGS84.GM / GEO_RADIUS_M)
  
  val GPS_RADIUS_M = { 
    //Was 26561.0 km, but not accurate enough
    val lhs = 0.25 * Time.EARTH_MEAN_SIDEREAL_DAY_SEC / Pi
    cbrt(lhs * lhs * WGS84.GM)
  }
  
  //was 26561.0 km/s
  val GPS_VELOCITY_M_PER_SEC = sqrt(WGS84.GM / GPS_RADIUS_M)
  
  val GEO_RADIUS_KM = GEO_RADIUS_M * 0.001
  val GEO_VELOCITY_KM_PER_SEC = GEO_VELOCITY_M_PER_SEC * 0.001
  
  val GPS_RADIUS_KM = GPS_RADIUS_M * 0.001
  val GPS_VELOCITY_KM_PER_SEC = GPS_VELOCITY_M_PER_SEC * 0.001
  val LEO_RADIUS_MIN_KM = 6678.0
  val LEO_RADIUS_MAX_KM = 7878.0
  val LEO_VELOCITY_MIN_KM_PER_SEC = 7.113
  val LEO_VELOCITY_MAX_KM_PER_SEC = 7.726

  val LEO_RADIUS_MIN_M = LEO_RADIUS_MIN_KM * 1000
  val LEO_RADIUS_MAX_M = LEO_RADIUS_MAX_KM * 1000
  val LEO_VELOCITY_MIN_M_PER_SEC = LEO_VELOCITY_MIN_KM_PER_SEC * 1000
  val LEO_VELOCITY_MAX_M_PER_SEC = LEO_VELOCITY_MAX_KM_PER_SEC * 1000
}
