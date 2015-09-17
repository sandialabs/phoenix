package gov.sandia.phoenix.attitudes

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry.Axes

/**
 * UTN is defined by these azes:
 * U - Up as defined by T x N
 * T - along track
 * N - Nadir (this is the defining vector)
 *
 * This should be used if following the STK default attitude model of "Nadir alignment with ECI velocity constraint."
 * http://www.agi.com/resources/help/online/stk/index.html?page=source%2Fextfile%2Fgator%2Feq-coordsys.htm
 */
object UTN extends AttitudeModel {
  def attitude(state : ECIStateVector) = Some(Axes.ZY(-state.position, state.velocity))
}
