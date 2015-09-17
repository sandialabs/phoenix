package gov.sandia.phoenix.propagators.sgp4

import scala.math._

trait LSConstants {
  def ω : Double
  def sω : Double
  def cω : Double
  def I : Double
  def sI : Double
  def cI : Double
  def sΩ : Double
  def cΩ : Double
  def eccentricity : Double
  def meanMotion : Double
  def C : Double
  def M(tsince : Double) : Double
  //Approximation for true anomaly
  def fx(tsince : Double) = {
    //The true anomaly of the perturbing body is approximated by fX = MX + 2eX sin MX (See Appendix A.E)
    val Mx = M(tsince)
    Mx + 2.0 * eccentricity * sin(Mx)
  }
}
