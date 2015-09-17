package gov.sandia.phoenix.elements.sv

import gov.sandia.phoenix.geometry.RRaDec

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
class RRaDecStateVector(val rradec : RRaDec, val drradec : RRaDec){
  override def toString = rradec + ", derivatives: " + drradec
}