package gov.sandia.phoenix.elements.sv

import gov.sandia.phoenix.geometry.Vector3
import gov.sandia.phoenix.time._

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
class TEMEStateVector(val position : Vector3, val velocity : Vector3) {
  def toECI(epoch : JD) = epoch.fk5.TEMEtoJ2000(this)
}