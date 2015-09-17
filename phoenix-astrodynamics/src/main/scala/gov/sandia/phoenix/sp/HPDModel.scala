package gov.sandia.phoenix.sp

import gov.sandia.phoenix.conf.SGConf
import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry.{RaDec, Vector3}

import scala.collection.immutable.SortedMap
import scala.language.postfixOps
import scala.math._

/**
 * The Harris-Priester Density Model.
 * 
 * See section 3.5.2 (p. 89) of Montenbruck & Gill.
 * 
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
object HPDModel {
  val modelRgx = """MODEL NAME: 1964 HARRIS/PRIESTER ATMOSPHERE \(MIN-MAX\) (\d+)-(\d+) KM  F=(\d+).*""".r
  val coefficients = load(URIUtil.toLineList(getClass.getResource(SGConf.HARRIS_PRIESTER_DENSITY_MODEL_FILE).toURI()))

  /**
   * h: Height of point above Earth's reference ellipsoid.
   * F: Solar flux.
   * radec: RaDec of the Sun.
   * state: State Vector to compute density position at.
   */
  def apply(h : Double, F : Double, radec : RaDec, state : ECIStateVector) = {
    val er = state.position.normalized
    //lag angle ~30 degrees
    val ralag = (radec.rightAscension.degrees + 30.0).toRadians
    val dec = radec.declination.radians
    val eb = new Vector3(cos(dec) * cos(ralag), cos(dec) * sin(ralag), sin(dec)).normalized
    val lowInc = 2.0
    val polInc = 6.0
    val n = lowInc + (polInc - lowInc) * state.inclination.radians / (Pi * 0.5)
    val cosn = pow(0.5 * (1.0 + eb * er), n)

    fluxCoefficients(F)(h, cosn)
  }

  def fluxCoefficients(F : Double) = if(F < coefficients.firstKey)
    coefficients(coefficients firstKey)
  else if(F > coefficients.lastKey)
    coefficients(coefficients lastKey)
  else {
    val lo = (coefficients to F.toInt).lastKey
    val hi = (coefficients from F.toInt).firstKey
    if(F - lo < hi - F) coefficients(lo) else coefficients(hi)
  }

  def load(s : List[String],
            activeKey : Int = -1,
            m : SortedMap[Int, HPDModel] = SortedMap.empty) : SortedMap[Int, HPDModel] = s match {
    case modelRgx(smin, smax, sf) :: tail => {
        val model = new HPDModel(sf.toDouble.toInt)
        load(tail, model.F, m+(model.F->model))
    }
    case HPDCoefficient(c) :: tail => load(tail, activeKey, m+(activeKey->(m(activeKey)+c)))
    case head :: tail => load(tail, activeKey, m)
    case Nil => m
  }
}

/**
 * The Harris-Priester Density Model.
 * 
 * See section 3.5.2 (p. 89) of Montenbruck & Gill.
 * 
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
class HPDModel(val F : Int, val m : SortedMap[Int, HPDCoefficient] = SortedMap.empty) {
  def +(c : HPDCoefficient) = new HPDModel(F, m+(c.h->c))
  override def toString = (("F: " + F + "\nalt, min, max") /: m)(_+ "\n" + _._2)

  def rho(rho_min : Double, rho_max : Double, cosn : Double) = rho_min + (rho_max - rho_min) * cosn

  def apply(h : Double, cosn : Double) = if(m contains h.toInt) {
    val r = m(h.toInt)
    rho(r.minRho, r.maxRho, cosn)
  } else if(h < m.firstKey || h > m.lastKey) 0.0 else {
    val selection = select(h)
    rho(rhoMin(selection, h), rhoMax(selection, h), cosn)
  }

  def select(h : Double) = (m((m to h.toInt).lastKey), m((m from h.toInt).firstKey))
  def H(f : HPDCoefficient => Double)(h : (HPDCoefficient, HPDCoefficient)) = (h._1.h - h._2.h) / log(f(h._2) / f(h._1))
  def rho(f : HPDCoefficient => Double)(h : (HPDCoefficient, HPDCoefficient), hx : Double) = f(h._1)*exp((h._1.h - hx) / H(f)(h))
  def rhoMin = rho(_.minRho)_
  def rhoMax = rho(_.maxRho)_
}

object HPDCoefficient {
  def unapply(s : String) = {
    val entries = s.trim.replaceAll("\\s+", ",").split(",")
    if(entries.length == 3 && allnumeric(entries.toList)) {
      Some(new HPDCoefficient(entries(0).toDouble.toInt*1000, entries(1).toDouble / 1.0E9, entries(2).toDouble / 1.0E9))
    } else None
  }

  private final def allnumeric(s : List[String]) : Boolean = s match {
    case Nil => true
    case head :: tail => if(!head.matches("""[E\d+-\.]+""")) false else allnumeric(tail)
  }
}

class HPDCoefficient(val h : Int, val minRho : Double, val maxRho : Double) extends Ordered[HPDCoefficient] {
  override def compare(that : HPDCoefficient) = this.h - that.h
  override def toString = h + ", " + minRho + ", " + maxRho
}