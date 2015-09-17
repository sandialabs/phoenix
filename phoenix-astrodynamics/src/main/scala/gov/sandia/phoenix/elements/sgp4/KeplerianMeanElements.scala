package gov.sandia.phoenix.elements.sgp4

import gov.sandia.phoenix.constants.Time
import gov.sandia.phoenix.elements.kepler.KeplerElements
import gov.sandia.phoenix.propagators.sgp4.GravityConstants
import gov.sandia.phoenix.time.JD

case class KeplerianMeanElements(keplers: KeplerElements, epoch : JD, bStarDrag: Double, gc: GravityConstants) extends MeanElements {
  def I0 = keplers.inclination.radians
  def Ω0 = keplers.OMEGA.radians
  def e0 = keplers.e
  def ω0 = keplers.omega.radians
  def meanAnomaly = keplers.meanAnomaly.value
  def meanMotion = keplers.n * Time.SEC_PER_MIN
}