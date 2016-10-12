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

package gov.sandia.phoenix

import gov.sandia.phoenix.time.JD
import java.util.logging.Logger

package object access {
  val logger = Logger.getLogger(getClass.getName)

  final def bisect(lo : (JD, Boolean), hi : (JD, Boolean), dt : Double, tol : Double, f : JD => Boolean) : JD = {
    require(lo._2 != hi._2)
    require(dt >= 0.0)
    val t = lo._1 mid hi._1
    if(dt <= tol) t else {
      val v = f(t)
      if (lo._2 == v) bisect((t, v), hi, dt * 0.5, tol, f) else bisect(lo, (t, v), dt * 0.5, tol, f)
    }
  }
}
