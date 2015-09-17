package gov.sandia.phoenix.sp

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry.Vector3
import gov.sandia.phoenix.numerics._
import gov.sandia.phoenix.numerics.optimization._
import gov.sandia.phoenix.propagators.Propagator
import gov.sandia.phoenix.time.JD

/**
 * A simple state based force propagator. No considerations for mass, fuel,
 * geometry, etc. are taken into account except for anything coded into the 
 * force model being used.
 * 
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
class FP(val epoch : JD,
         val initialState : ECIStateVector,
         val forcemodel : SPModel = SMGModel+SRP(0.01, 0.85)) extends Propagator {
  val controller = new UnboundedStepSizeController(1.0)
  val solver = RKDP_8_7.createSolver(dfdx, new UnboundedStepSizeController(1.0))
  val initialConditions = (0.0, initialState.toArray : IndexedSeq[Double])

  def state(t : JD) = Some {
    val solution = solve(t)
    val lastDt = solution.lastKey
    val s = solution(lastDt)
    ECIStateVector(Vector3(s(0), s(1), s(2)), Vector3(s(3), s(4), s(5)))
  }
  
  def solve(t : JD)= {
    val dt = if(t > epoch) (epoch until t).getDurationSeconds else -(t until epoch).getDurationSeconds
    solver(initialConditions, dt.doubleValue, 600)
  }
  
  val rangeFcn : Double => Double = { t => unsafe_state(epoch plusSeconds t).position.mag }
  val rateFcn : Double => Double = { t => 
    val s = unsafe_state(epoch plusSeconds t)
    s.position.normalized * s.velocity
  }
  
  def perigee = goldenMin(0, initialState.period, 0.1)(rangeFcn)
  def apogee = goldenMax(0, initialState.period, 0.1)(rangeFcn)
  
  def dfdx(t : Double, y : IndexedSeq[Double]) = {
    val state = ECIStateVector(Vector3(y(0), y(1), y(2)), Vector3(y(3), y(4), y(5)))
    val f = forcemodel ΣF (epoch plusSeconds t, state)
    Array(y(3), y(4), y(5), f.x, f.y, f.z)
  }
}