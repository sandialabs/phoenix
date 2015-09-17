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
