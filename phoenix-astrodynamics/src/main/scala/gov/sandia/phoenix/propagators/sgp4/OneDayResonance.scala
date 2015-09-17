package gov.sandia.phoenix.propagators.sgp4

import scala.math._
import scala.Some
import gov.sandia.phoenix.elements.sgp4.MeanElements

class OneDayResonance(val eleset : MeanElements) extends ResonanceTerms {
  val del1 = OneDayResonance.dels(1, eleset.ao, eleset.no, eleset.e0, eleset.I0)
  val del2 = OneDayResonance.dels(2, eleset.ao, eleset.no, eleset.e0, eleset.I0)
  val del3 = OneDayResonance.dels(3, eleset.ao, eleset.no, eleset.e0, eleset.I0)
  //See Appendix D
  val xlamo = SGP4Util.mod2pi(eleset.meanAnomaly + eleset.Ω0 + eleset.ω0 - SGP4Util.mod2pi(eleset.gmst))
  val xfact = eleset.dM + eleset.dω + eleset.dΩ - SGP4Util.rptim + eleset.dmdt + eleset.domdt + eleset.dnodt - eleset.no
  val frequency = 1

  def update(atime : Double, xli : Double, xni : Double) = {
    val xndt = del1 * sin(xli - fasx2) + del2 * sin(2.0 * (xli - fasx4)) + del3 * sin(3.0 * (xli - fasx6))
    val xldot = xni + xfact
    val xnddt = del1 * cos(xli - fasx2) + 2.0 * del2 * cos(2.0 * (xli - fasx4)) + 3.0 * del3 * cos(3.0 * (xli - fasx6))
    (xndt, xldot, xnddt * xldot)
  }
}

object OneDayResonance {
//  def unapply(eleset : ElementSetRecord) = Angle.TwoPi.radians / eleset.no match {
//    case period if period >= 1200 && period <= 1800 => Some(new OneDayResonance(eleset))
//    case _ => None
//  }

  def unapply(eleset : MeanElements) = eleset.no match {
    case no if no < 0.0052359877 && no > 0.0034906585 => Some(new OneDayResonance(eleset))
    case _ => None
  }

  def F(l : Int, m : Int, p : Int, i0 : Double) = {
    val si = sin(i0)
    val ci = cos(i0)
    val cp1 = ci + 1.0
    val s2 = si * si

    (l, m, p) match {
      case (2, 2, 0) => 0.75 * cp1 * cp1
      case (3, 1, 1) => 0.9375 * s2 * (1.0 + 3.0 * ci) - 0.75 * cp1
      case (3, 3, 0) => 1.875 * cp1 * cp1 * cp1
      case _ =>
        println("F: " + l + ", " + m + ", " + p)
        0.0
    }
  }

  def G(l : Int, p : Int, q : Int, e0 : Double) = {
    val e2 = e0 * e0
    (l, p, q) match {
      case (2, 0, 0) => 1.0 - (2.5 - 13.0 / 16.0 * e2) * e2
      case (3, 1, 0) => 1.0 + 2.0 * e2
      case (3, 0, 0) => 1.0 - (6.0 - 423.0 / 64.0 * e2) * e2
      case _ =>
        println("G: " + l + ", " + p + ", " + q)
        0.0
    }
  }

  def Q(l : Int, p : Int) = (l, p) match {
    case (3, 1) => 2.1460748E-6
    case (2, 2) => 1.7891679E-6
    case (3, 3) => 2.2123015E-7
    case _ =>
      println("Q: " + l + ", " + p)
      0.0
  }

  def dels(i : Int, a0 : Double, n0 : Double, e0 : Double, i0 : Double) = {
    def n2a2 = n0 * n0 / (a0 * a0)
    i match {
      case 1 => 3.0 * n2a2 / a0 * F(3, 1, 1, i0) * G(3, 1, 0, e0) * Q(3, 1)
      case 2 => 6.0 * n2a2 * F(2, 2, 0, i0) * G(2, 0, 0, e0) * Q(2, 2)
      case 3 => 9.0 * n2a2 / a0 * F(3, 3, 0, i0) * G(3, 0, 0, e0) * Q(3, 3)
      case _ =>
        println("del: " + i)
        0.0
    }
  }
}