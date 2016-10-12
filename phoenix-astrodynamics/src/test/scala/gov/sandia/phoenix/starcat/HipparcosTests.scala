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

package gov.sandia.phoenix.starcat

import org.scalatest.FunSuite
import gov.sandia.phoenix.geometry.RaDec
import gov.sandia.phoenix.geometry.RightAscension
import gov.sandia.phoenix.geometry.Declination
import scala.math._
import gov.sandia.phoenix.starcat.hipparcos.HipparcosCatalog
import scala.language.postfixOps

/**
 * @author <A HREF="mailto:markbastian@gmail.com">Mark Bastian</A>
 *
 */
class HipparcosTests extends FunSuite {
  val cat = HipparcosCatalog.default

  test("A circularly swept area should have more stars than a circumscribed rectangular area.") {
    val mi = new RaDec(RightAscension.fromDegrees(45.0), Declination.fromDegrees(-23.0))
    val ma = new RaDec(RightAscension.fromDegrees(50.0), Declination.fromDegrees(-17.0))
    val stars = cat.starsIn(mi, ma)

    val boresight = mi mid ma toECI
    val fov = acos(mi.toECI * ma.toECI).toDegrees
    val test = cos(fov.toRadians * 0.5)
    val coses = cat.entries map { _.meanRaDec.toECI * boresight } filter { _ >= test }

    assert(coses.size >= stars.size)
    assert(coses.size === cat.starsInFov(boresight, fov).size)
  }

  test("Big Dipper Filter.") {
    val mi = new RaDec(RightAscension.fromDegrees(11.0 / 24.0 * 360), Declination.fromDegrees(47))
    val ma = new RaDec(RightAscension.fromDegrees(14.0 / 24.0 * 360), Declination.fromDegrees(63))
    val starsRaDec = cat.starsIn(mi, ma) filter { _.visualMagnitude < 3.4f }

    val boresight = mi mid ma toECI
    val fov = acos(mi.toECI * ma.toECI).toDegrees
    val starsFOV = cat.starsInFov(boresight, fov) filter { _.visualMagnitude < 3.4f }
    
    println(starsRaDec.size)
    println(starsFOV.size)
    assert(starsFOV.size >= starsRaDec.size)
    assert(starsFOV union starsRaDec sameElements starsRaDec)
  }
}