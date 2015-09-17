package gov.sandia.phoenix.propagators.sgp4

import scala.math._
import scala.Some
import gov.sandia.phoenix.elements.sgp4.MeanElements

class HalfDayResonance(val eleset : MeanElements) extends ResonanceTerms {
  val d2201 = D(2, 2, 0, 1, eleset.ao, eleset.no, eleset.e0, eleset.I0)
  val d2211 = D(2, 2, 1, 1, eleset.ao, eleset.no, eleset.e0, eleset.I0)
  val d3210 = D(3, 2, 1, 0, eleset.ao, eleset.no, eleset.e0, eleset.I0)
  val d3222 = D(3, 2, 2, 2, eleset.ao, eleset.no, eleset.e0, eleset.I0)
  val d4410 = D(4, 4, 1, 0, eleset.ao, eleset.no, eleset.e0, eleset.I0)
  val d4422 = D(4, 4, 2, 2, eleset.ao, eleset.no, eleset.e0, eleset.I0)
  val d5220 = D(5, 2, 2, 0, eleset.ao, eleset.no, eleset.e0, eleset.I0)
  val d5232 = D(5, 2, 3, 2, eleset.ao, eleset.no, eleset.e0, eleset.I0)
  val d5421 = D(5, 4, 2, 1, eleset.ao, eleset.no, eleset.e0, eleset.I0)
  val d5433 = D(5, 4, 3, 3, eleset.ao, eleset.no, eleset.e0, eleset.I0)
  //See Appendix D
  val xlamo = SGP4Util.mod2pi(eleset.meanAnomaly + 2.0 * (eleset.Ω0 - SGP4Util.mod2pi(eleset.gmst)))
  val xfact = eleset.dM + eleset.dmdt + 2.0 * (eleset.dΩ + eleset.dnodt - SGP4Util.rptim) - eleset.no
  val frequency = 2

  def update(atime : Double, xli : Double, xni : Double) = {
    val xomi = eleset.ω0 + eleset.dω * atime
    val x2omi = xomi + xomi
    val x2li = xli + xli

    val xndt = d2201 * sin(x2omi + xli - G22) +
      d2211 * sin(xli - G22) + d3210 * sin(xomi + xli - G32) +
      d3222 * sin(-xomi + xli - G32) + d4410 * sin(x2omi + x2li - G44) +
      d4422 * sin(x2li - G44) + d5220 * sin(xomi + xli - G52) +
      d5232 * sin(-xomi + xli - G52) + d5421 * sin(xomi + x2li - G54) +
      d5433 * sin(-xomi + x2li - G54)

    val xldot = xni + xfact
    val xnddt = d2201 * cos(x2omi + xli - G22) +
      d2211 * cos(xli - G22) + d3210 * cos(xomi + xli - G32) +
      d3222 * cos(-xomi + xli - G32) + d5220 * cos(xomi + xli - G52) +
      d5232 * cos(-xomi + xli - G52) + 2.0 * (d4410 * cos(x2omi + x2li - G44) +
      d4422 * cos(x2li - G44) +
      d5421 * cos(xomi + x2li - G54) +
      d5433 * cos(-xomi + x2li - G54))
    (xndt, xldot, xnddt * xldot)
  }
}

object HalfDayResonance {
//  def unapply(eleset : ElementSetRecord) = Angle.TwoPi.radians / eleset.no match {
//    case period if period >= 680 && period <= 760 && eleset.e0 >= 0.5 =>
//      Some(new HalfDayResonance(eleset))
//    case _ => None
//  }

  def unapply(eleset : MeanElements) = eleset.no match {
    case no if no >= 8.26e-3 && no <= 9.24e-3 && eleset.e0 >= 0.5 =>
      Some(new HalfDayResonance(eleset))
    case _ => None
  }
}
