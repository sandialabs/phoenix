/**
 *
 */
package gov.sandia.phoenix.elements.sv

import gov.sandia.phoenix.geometry.{AzElR, Vector3}

import scala.math._


/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 *
 */
case class SEZStateVector(sez : Vector3, dsez : Vector3) {
  def toECEF(ecef : Vector3) = {
    val frame = ecef.toGeodetic.toSEZFrame
    new ECEFStateVector(frame * sez, frame.rotation * dsez)
  }
  
  def toAzElR = {
    val pAzElR = sez.toAzElR
    val dR = sez * dsez / sez.mag
    val den = sez.x * sez.x + sez.y * sez.y
    val dAz = (dsez.x * sez.y - dsez.y * sez.x) / den
    val dEl = (dsez.z - dR * sin(pAzElR.elevation.toRadians)) / sqrt(den)

    new AzElRStateVector(pAzElR, new AzElR(dAz.toDegrees, dEl.toDegrees, dR))
  }

  def pretty = sez + ", " + dsez
}