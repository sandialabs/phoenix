package gov.sandia.phoenix.propagators.sgp4

import scala.math._
import SGP4Util._
import gov.sandia.phoenix.elements.sgp4.MeanElements

case class DeepSpaceArgs(e0: Double, ω0: Double, I0: Double, M0: Double, Ω0: Double, n0: Double) {
  def this(satrec: MeanElements, Δt: Double) =
    this(satrec.e0, satrec.ω0 + satrec.dω * Δt, satrec.I0, satrec.meanAnomaly + satrec.dM * Δt,
      satrec.Ω0 + Δt *
        (satrec.dΩ + (3.5 * satrec.β0 * satrec.β0 * -1.5 * satrec.gc.j2 * satrec.no * satrec.θ * satrec.C1 / satrec.a2β4) * Δt),
      satrec.no)

  def finalConvert(satrec: MeanElements, tempa: Double, tempe: Double, templ: Double, am: Double) = {
    val emTol = -1.0E-3
    if ((e0 - tempe >= 1.0) || (e0 - tempe < emTol) || (am < 0.95)) {
      satrec.error = "mean elements, ecc >= 1.0 or ecc < " + emTol + " (" + e0 + ") or a < 0.95 er (" + am + ")"
      logger.severe(satrec.error)
    }

    new DeepSpaceArgs(max(e0 - tempe, 1.0e-6), mod2pi(ω0), I0,
      mod2pi(mod2pi(M0 + satrec.no * templ + ω0 + Ω0) - ω0 - Ω0),
      mod2pi(Ω0), satrec.gc.xke / pow(am, 1.5))
  }

  /**
   * Euler-Maclaurin integration as described in Appendix A.D.
   */
  def resonanceUpdate(satrec: MeanElements, Δt: Double) = {
    val e = e0 + satrec.dedt * Δt
    val ω = ω0 + satrec.domdt * Δt
    val i = I0 + satrec.didt * Δt
    val Ω = Ω0 + satrec.dnodt * Δt
    if (satrec.resonanceTerms.frequency != 0) {
      def integrate(atime: Double, xli: Double, xni: Double, xndt: Double, xnddt: Double):
      (Double, Double, Double, Double, Double, Double) = {
        val (xndt0, xldot0, xnddt0) = satrec.resonanceTerms.update(atime, xli, xni)
        if (abs(Δt - atime) >= stepp) integrate(atime + stepp * signum(Δt),
          xli + xldot0 * stepp * signum(Δt) + xndt0 * step2, xni + xndt0 * stepp * signum(Δt) + xnddt0 * step2, xndt0, xnddt0)
        else (xli, xni, xndt0, xldot0, xnddt0, Δt - atime)
      }

      val (xli, xni, xndt, xldot, xnddt, ft) = integrate(0.0, satrec.resonanceTerms.xlamo, satrec.no, 0.0, 0.0)
      val λi = xli + (xldot + xndt * 0.5 * ft) * ft
      val ni = xni + (xndt + xnddt * ft * 0.5) * ft
      val θ = mod2pi(satrec.gmst + Δt * rptim)
      if (satrec.resonanceTerms.frequency == 1)
        new DeepSpaceArgs(e, ω, i, λi - Ω - ω + θ, Ω, ni)
      else new DeepSpaceArgs(e, ω, i, λi - 2.0 * Ω + 2.0 * θ, Ω, ni)
    } else new DeepSpaceArgs(e, ω, i, M0 + satrec.dmdt * Δt, Ω, n0)
  }
}