package gov.sandia.phoenix.propagators.sgp4

import scala.math._

/**
 * Compute LuniSolar Coefficients for SGP4.
 * See Appendix C of A HISTORY OF ANALYTICAL ORBIT MODELING IN THE UNITED STATES SPACE SURVEILLANCE SYSTEM by Hoots, et al.
 * http://aero.tamu.edu/sites/default/files/faculty/alfriend/S6.1%20Hoots.pdf
 * Or Appendix A of the same article at http://arc.aiaa.org/doi/pdf/10.2514/1.9161.
 */
case class LSCoefficients(x : LSConstants, Ω0: Double, I0 : Double, ω0 : Double, e0: Double, n0: Double) {
  private val cΩ0 = cos(Ω0)
  private val sΩ0 = sin(Ω0)
  private val cdΩ = cΩ0 * x.cΩ + sΩ0 * x.sΩ
  private val sdΩ = sΩ0 * x.cΩ - cΩ0 * x.sΩ
  private val cI0 = cos(I0)
  private val sI0 = sin(I0)
  private val cω0 = cos(ω0)
  private val sω0 = sin(ω0)

  private val a1 = x.cω * cdΩ + x.sω * x.cI * sdΩ
  private val a3 = -x.sω * cdΩ + x.cω * x.cI * sdΩ
  private val a7 = -x.cω * sdΩ + x.sω  * x.cI * cdΩ
  private val a8 = x.sω * x.sI
  private val a9 = x.sω * sdΩ + x.cω * x.cI * cdΩ
  private val a10 = x.cω * x.sI
  private val a2 = a7 * cI0 + a8 * sI0
  private val a4 = a9 * cI0 + a10 * sI0
  private val a5 = -a7 * sI0 + a8 * cI0
  private val a6 = -a9 * sI0 + a10 * cI0

  private val X1 = a1 * cω0 + a2 * sω0
  private val X2 = a3 * cω0 + a4 * sω0
  private val X3 = -a1 * sω0 + a2 * cω0
  private val X4 = -a3 * sω0 + a4 * cω0
  private val X5 = a5 * sω0
  private val X6 = a6 * sω0
  private val X7 = a5 * cω0
  private val X8 = a6 * cω0

  private val Z31 = 12.0 * X1 * X1 - 3.0 * X3 * X3
  private val Z32 = 24.0 * X1 * X2 - 6.0 * X3 * X4
  private val Z33 = 12.0 * X2 * X2 - 3.0 * X4 * X4

  private val Z11 = -6.0 * a1 * a5 + e0 * e0 * (-24.0 * X1 * X7 - 6.0 * X3 * X5)
  private val Z13 = -6.0 * a3 * a6 + e0 * e0 * (-24.0 * X2 * X8 - 6.0 * X4 * X6)
  private val Z21 = 6.0 * a2 * a5 + e0 * e0 * (24.0 * X1 * X5 - 6.0 * X3 * X7)
  private val Z23 = 6.0 * a4 * a6 + e0 * e0 * (24.0 * X2 * X6 - 6.0 * X4 * X8)
  private val Z22 = 6.0 * (a4 * a5 + a2 * a6) + e0 * e0 * (24.0 * (X2 * X5 + X1 * X6) - 6.0 * (X4 * X7 + X3 * X8))
  private val Z12 = -6.0 * (a1 * a6 + a3 * a5) + e0 * e0 * (-24.0 * (X2 * X7 + X1 * X8) - 6.0 * (X3 * X6 + X4 * X5))

  private val Z1 = 6.0 * (a1 * a1 + a2 * a2) + Z31 * e0 * e0 + Z31
  private val Z2 = 12.0 * (a1 * a3 + a2 * a4) + Z32 + Z32 * e0 * e0
  private val Z3 = 6.0 * (a3 * a3 + a4 * a4) + Z33 + Z33 * e0 * e0

  private val eta0 = sqrt(1.0 - e0 * e0)
  private val CIx = 1.0 / (2.0 * n0 * eta0)
  private val Cn = x.C * x.meanMotion

  //Secular rate contribution from this third-body
  val de = -15.0 * e0 * eta0 / n0 * Cn * (X1 * X3 + X2 * X4)
  val dI = - CIx * Cn * (Z11 + Z13)
  val dM = - Cn * (Z1 + Z3 - 14.0 - 6.0 * e0 * e0) / n0
  val dΩ = if(I0 < 5.2359877e-2 || I0 > Pi - 5.2359877e-2) 0.0 else CIx * Cn * (Z21 + Z23) / sI0
  val dω = Cn * eta0 / n0 * (Z31 + Z33 - 6.0) + (if(I0 < 5.2359877e-2 || I0 > Pi - 5.2359877e-2) 0.0 else - dΩ * cI0)

  /**
   * Appendix A.E (Two column format)
   */
  def periodics(tsince : Double) = {
    val fx = x.fx(tsince)
    val F2 = 0.5 * sin(fx) * sin(fx) - 0.25
    val F3 = -0.5 * sin(fx) * cos(fx)
    val dex = -30.0 * e0 * x.C * eta0 * (F2 * (X2 * X3 + X1 * X4) + F3 * (X2 * X4 - X1 * X3)) / n0
    val dIx = -x.C * (F2 * Z12 + F3 * (Z13 - Z11)) / (n0 * eta0)
    val dMx = -2.0 * x.C * (F2 * Z2 + F3 * (Z3 - Z1) - 3.0 * x.eccentricity * sin(fx) * (7.0 + 3.0 * e0 * e0)) / n0
    val dωcIdΩx = 2.0 * x.C * eta0 * (Z32 * F2 + (Z33 - Z31) * F3 - 9.0 * x.eccentricity * sin(fx)) / n0
    val sIxdΩx = x.C * (F2 * Z22 + F3 * (Z23 - Z21)) / (n0 * eta0)
    Array(dex, dIx, dMx, dωcIdΩx, sIxdΩx)
  }
}