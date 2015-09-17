package gov.sandia.phoenix.sp

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.math._
import gov.sandia.phoenix.solarsystem.Sol
import gov.sandia.phoenix.time.JD

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class DragForce(dragCoefficient : Double = 2.2, areaToMassRatio : Double = 0.02) extends SPForceProvider {
  def acceleration(t : JD, state : ECIStateVector) = {
    val rho = HPDModel(t.ECItoECEF(state.position).toGeodetic.elevation, SolarFlux.fit(t),
      ECItoRaDec(Sol.eciPosition(t)), state)
    Drag.drag(dragCoefficient, areaToMassRatio, rho, state)
  }
}