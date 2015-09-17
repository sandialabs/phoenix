package gov.sandia.phoenix.sp
import scala.language.implicitConversions

/**
 * I,,sp,,=v,,e,,/9.81 m/s^2^
 * 
 * F=|ṁ|v,,e,,
 */
trait ThrustSystem {
  def force : Double
  def exitVelocity : Double
  def specificImpulse : Double
  def massFlowRate : Double
}

//class BasicThruster(val exitVelocity : Double, val massFlowRate : Double) extends ThrustSystem {
//  val force = massFlowRate * exitVelocity
//  val specificImpulse = exitVelocity / 9.81
//}

//object DefaultSolidPropellantBoostMotor extends BasicThruster(3000, 1.3)
//object LiquidSolidPropellantBoostMotor extends BasicThruster(3500, 130E-3)
//object DefaultStationKeepingThruster extends BasicThruster(3500, 3E-3)
//object DefaultIonThruster extends BasicThruster(25000, 0.8E-6)

class ThrusterModel(val force : Double, val exitVelocity : Double, val specificImpulse : Double, val massFlowRate : Double)
