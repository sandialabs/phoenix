package gov.sandia.phoenix.propagators.sgp4

import gov.sandia.phoenix.elements.sgp4.MeanElements

object ResonanceTermsSwitcher {
  def getType(satrec : MeanElements) = satrec match {
    case HalfDayResonance(resonance) => resonance
    case OneDayResonance(resonance) => resonance
    case _ => NoResonance
  }
}
