package gov.sandia.phoenix.sp

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.solarsystem.{Luna, Sol}
import gov.sandia.phoenix.time.JD

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
class SPModel(val forces : Set[SPForceProvider] = Set.empty) {

  def ΣF(t : JD, state : ECIStateVector) = forces map { _.acceleration(t, state) } reduceLeft { (a, b) => a+b }

  def sigmaF (t : JD, state : ECIStateVector) =  ΣF (t, state)
  
  def +(force : SPForceProvider) = new SPModel(forces+force)
  
  override def toString = ("Forces:" /: forces){ (s, f) => s + "\n\t" + f }
}

/**
 * Sun, Moon, Earth Gravity Model. The following forces are present:
 * Sun: Low precision Sun location.
 * Moon: JPL405 Moon ephemeris.
 * Earth Gravity: EGM96 model.
 * 
 * This is a good model to start with. If SRP or Drag are desired they can be
 * added with their respective parameters for whatever vehicle is to be modeled.
 */
object SMGModel extends SPModel(Set(Sol, Luna, DefaultEGM96GravityForce))