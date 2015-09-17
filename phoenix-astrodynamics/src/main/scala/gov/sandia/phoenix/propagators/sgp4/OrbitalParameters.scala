package gov.sandia.phoenix.propagators.sgp4

import java.util.logging.Logger
import scala.math._
import SGP4Util._
import gov.sandia.phoenix.elements.sgp4.MeanElements

object OrbitalParameters {
  val logger = Logger.getLogger(getClass.getName)
  def apply(satrec : MeanElements, Δt : Double) = {
    /* ------- update for secular gravity and atmospheric drag ----- */
    var dspaceargs = new DeepSpaceArgs(satrec, Δt)
    dspaceargs = satrec.update(dspaceargs, Δt)

    //4. Secular Update for Remaining Atmospheric Drag Effects
    val f = if(satrec.isimp != 1) 1 else 0
    //For these two we don't actually need the f since everything is zeroed any ways.
    val a = 1.0 - (satrec.C1 + f * (satrec.D2 + (satrec.D3 + satrec.D4 * Δt) * Δt) * Δt) * Δt
    val ILx = (1.5 * satrec.C1 + f * (satrec.t3cof + (satrec.t4cof + satrec.t5cof * Δt) * Δt) * Δt) * Δt * Δt
    val e = satrec.bStarDrag * satrec.C4 * Δt + f * satrec.bStarDrag * satrec.C5 * (sin(dspaceargs.M0) - sin(satrec.meanAnomaly))

    dspaceargs = satrec.LuniSolarUpdate(dspaceargs, Δt)
        
    if (dspaceargs.n0 <= 0.0) {
      satrec.error = "mean motion < 0 (" + dspaceargs.n0 + ")"
      logger.severe(satrec.error)
    }
        
    val am = pow(satrec.gc.xke / dspaceargs.n0, 2.0 / 3.0) * a * a
        
    dspaceargs = dspaceargs.finalConvert(satrec, a, e, ILx, am)

    /* -------------------- add lunar-solar periodics -------------- */
    new OrbitalParameters(dspaceargs, am)
  }
  
  /* --------------------- solve kepler's equation --------------- */
  private final def solveKep(u : Double, aynl : Double, axnl : Double) : (Double, Double) = 
  solveKep(u, aynl, axnl, u, 9999.9, 1, 0, 0)
  
  /* --------------------- solve kepler's equation --------------- */
  private final def solveKep(u : Double, aynl : Double, axnl : Double, 
                             eo1 : Double, tem5 : Double, ktr : Int,
                             sineo1 : Double, coseo1 : Double) : (Double, Double) =
  if(!((abs(tem5) >= 1.0e-12) && (ktr <= 10))) (sineo1, coseo1) else {
    val s = sin(eo1)
    val c = cos(eo1)
    val t = (u - aynl * c + axnl * s - eo1) / (1.0 - c * axnl - s * aynl)
    val tt = if(abs(t) >= 0.95) if(t > 0.0) 0.95 else -0.95 else t
    solveKep(u, aynl, axnl, eo1 + tt, tt, ktr + 1, s, c)
  }
}

class OrbitalParameters(val eccentricity : Double, val inclination : Double, val rightAscension : Double,
                        val argumentOfPerigee : Double, val meanAnomaly : Double, val meanMotion : Double, val am : Double) {
  val sinip = sin(inclination)
  val cosip = cos(inclination)

  def this(args : DeepSpaceArgs, am : Double) =
  this(args.e0, args.I0, args.Ω0, args.ω0, args.M0, args.n0, am)

  def deepSpaceConvert(p : Periodics, Δt : Double) =
  {
    val incl = inclination + p.pinc
    val ecc = eccentricity + p.pe
    val sinip = sin(incl)
    val cosip = cos(incl)

    var argp = argumentOfPerigee
    var raan = rightAscension
    var M0 = meanAnomaly
    /* ----------------- apply periodics directly ------------ */
    // sgp4fix for lyddane choice
    // strn3 used original inclination - this is technically feasible
    // gsfc used perturbed inclination - also technically feasible
    // probably best to readjust the 0.2 limit value and limit
    // discontinuity
    // 0.2 rad = 11.45916 deg
    // use next line for original strn3 approach and original
    // inclination
    // if (inclo >= 0.2)
    // use next line for gsfc version and perturbed inclination

    //Appendix B.B.5 - Update for Long-Period Periodic Effects of Lunar and Solar Gravity
    if(incl >= 0.2)
    {
      argp = argumentOfPerigee + (p.pgh - cosip * p.ph / sinip)
      raan = rightAscension + p.ph / sinip
      M0 = meanAnomaly + p.pl
    }
    else
    {
      /* ---- apply periodics with lyddane modification ---- */
      val sinop = sin(rightAscension)
      val cosop = cos(rightAscension)
      val dalf = p.ph * cosop + p.pinc * cosip * sinop
      val dbet = -p.ph * sinop + p.pinc * cosip * cosop
      val alfdp = sinip * sinop + dalf
      val betdp = sinip * cosop + dbet

      raan = mod2pi(rightAscension)
      raan = if(raan < 0) raan + 2.0 * Pi else raan
      val dls = p.pl + p.pgh - p.pinc * raan * sinip
      val xls = meanAnomaly + argumentOfPerigee + cosip * raan + dls

      val xnoh = raan
      raan = atan2(alfdp, betdp)
      raan = if(raan < 0) raan + 2.0 * Pi else raan
      if(abs(xnoh - raan) > Pi)
      if(raan < xnoh)
      raan = raan + 2.0 * Pi
      else
      raan = raan - 2.0 * Pi
      M0 = meanAnomaly + p.pl
      argp = xls - M0 - cosip * raan
    }

    if(incl >= 0) new OrbitalParameters(ecc, incl, raan, argp, M0, meanMotion, am) else
      new OrbitalParameters(ecc, -incl, raan + Pi, argp - Pi, M0, meanMotion, am)
  }
}
