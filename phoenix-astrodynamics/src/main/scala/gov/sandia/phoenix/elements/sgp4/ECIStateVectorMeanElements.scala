package gov.sandia.phoenix.elements.sgp4

import gov.sandia.phoenix.constants.Time
import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.propagators.sgp4.GravityConstants
import gov.sandia.phoenix.time.JD

case class ECIStateVectorMeanElements(state: ECIStateVector, epoch: JD, bStarDrag: Double, gc: GravityConstants) extends MeanElements {
  def I0 = state.keplers.inclination.radians
  def Ω0 = state.keplers.OMEGA.radians
  def e0 = state.keplers.e
  def ω0 = state.keplers.omega.radians
  def meanAnomaly = state.keplers.meanAnomaly.value
  def meanMotion = state.keplers.n * Time.SEC_PER_MIN
}