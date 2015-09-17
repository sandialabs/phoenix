package gov.sandia.phoenix.propagators

import gov.sandia.phoenix.constants._
import gov.sandia.phoenix.time._
import gov.sandia.phoenix.elements.kepler.KeplerElements

class KeplerElementsPropagator(val elements : KeplerElements,
                               val epoch : JD) extends Propagator {
  def state(t : JD) = Some(elements.state((t.doubleValue - epoch.doubleValue) * Time.SEC_PER_DAY))
    
  def getPeriod = elements.period
  def getRAAN = elements.OMEGA
  def getArgP = elements.omega
  def getMeanAnomaly = elements.meanAnomaly.value
  def getSemimajorAxis = elements.a
}