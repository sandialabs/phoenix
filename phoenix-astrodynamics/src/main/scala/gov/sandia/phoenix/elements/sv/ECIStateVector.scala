package gov.sandia.phoenix.elements.sv

import gov.sandia.phoenix.constants._
import gov.sandia.phoenix.elements.ElementFunctions
import gov.sandia.phoenix.elements.kepler.{CircularEquatorialKeplerElements, CircularInclinedKeplerElements, EllipticalEquatorialKeplerElements, EllipticalInclinedKeplerElements}
import gov.sandia.phoenix.elements.util.Jacobians
import gov.sandia.phoenix.geometry._
import gov.sandia.phoenix.orbits._
import gov.sandia.phoenix.solarsystem.Sol
import gov.sandia.phoenix.time._

import scala.collection.immutable._
import scala.math._

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class ECIStateVector(position: Vector3, velocity: Vector3)
  extends CartesianStateVector with ElementFunctions{
  def toECEF(epoch: JD) = epoch.fk5.J2000toITRF(this)
  def toTOD(epoch: JD) = epoch.fk5.J2000toTOD(this)

  def toRRaDec = {
    val range = position.mag
    val den = position.x * position.x + position.y * position.y
    val dec = atan2(position.z, sqrt(den)).toDegrees
    val ra = (if (den != 0.0) atan2(position.y, position.x) else atan2(velocity.y, velocity.x)).toDegrees

    val rangeRate = position * velocity / range
    val dra = ((velocity.y * position.x - position.y * velocity.x) / den).toDegrees
    val ddec = ((velocity.z - rangeRate * position.z / range) / sqrt(den)).toDegrees
    new RRaDecStateVector(new RRaDec(range, ra, dec), new RRaDec(rangeRate, dra, ddec))
  }

  def +(that: ECIStateVector) = ECIStateVector(this.position + that.position, this.velocity + that.velocity)
  def -(that: ECIStateVector) = ECIStateVector(this.position - that.position, this.velocity - that.velocity)
  def *(s: Double) = ECIStateVector(this.position * s, this.velocity * s)
  def /(s: Double) = this * (1.0 / s)

  def circularize = {
    val inclination = Radians(acos(h.z / h.mag))
    val p = position.mag
    val n = K_HAT % h
    val r = position
    if (inclination.radians == 0) {
      val true_longitude = Radians(if (r.y < 0) 2 * Pi - acos(r.x / r.mag) else acos(r.x / r.mag))
      new CircularEquatorialKeplerElements(p, true_longitude).state
    } else {
      val OMEGA = Radians(if (n.y < 0) 2 * Pi - acos(n.x / n.mag) else acos(n.x / n.mag))
      val u = Radians(if (r.z < 0) 2 * Pi - acos(n * r / (n.mag * r.mag)) else acos(n * r / (n.mag * r.mag)))
      new CircularInclinedKeplerElements(p, inclination, OMEGA, u).state
    }
  }

  def topocentric(obs: ECIStateVector) = {
    val rangeVec = obs.position - position
    val range = rangeVec.mag
    val dec = asin(rangeVec.z / range).toDegrees
    val den = rangeVec.x * rangeVec.x + rangeVec.y * rangeVec.y
    val rangeRateVec = obs.velocity - velocity
    val ra = (if (den != 0.0) atan2(rangeVec.y, rangeVec.x) else atan2(rangeRateVec.y, rangeRateVec.x)).toDegrees

    val rangeRate = rangeVec * rangeRateVec / range
    val dra = ((rangeRateVec.y * rangeVec.x - rangeVec.y * rangeRateVec.x) / den).toDegrees
    val ddec = ((rangeRateVec.z - rangeRate * rangeVec.z / range) / sqrt(den)).toDegrees
    new RRaDecStateVector(new RRaDec(range, ra, dec), new RRaDec(rangeRate, dra, ddec))
  }

  override def keplers = toKeplerElements
  /**
   * See ELORB, p. 120 of Vallado
   */
  lazy val toKeplerElements = {
    val r = position
    val v = velocity
    val rv = r * v
    val vv = v * v
    val rmag = r.mag
    val hmag = h.mag
    //Node vector
    val n = K_HAT % h
    val nmag = n.mag
    val mu = WGS84.GM

    //Semi-parameter
    val p = h * h / mu

    val ē = (r * (vv - mu / rmag) - v * rv) / mu
    val e = ē.mag

    val inclination = Radians(acos(h.z / hmag))
    val nu = Radians(if (e != 0) {
      val cNu = min(max((ē * r) / (e * rmag), -1), 1.0)
      if (rv < 0 && cNu < 1.0) 2 * Pi - acos(cNu) else acos(cNu)
    } else Double.NaN)

    if (inclination.radians != 0.0) {
      val cosΩ = min(max(n.x / nmag, -1.0), 1.0)
      val Ω = Radians(if (n.y < 0) 2 * Pi - acos(cosΩ) else acos(cosΩ))
      if (e != 0.0) {
        val cos_ω = min(max(n * ē / (nmag * e), -1.0), 1.0)
        val ω = Radians(if (ē.z < 0) 2 * Pi - acos(cos_ω) else acos(cos_ω))
        new EllipticalInclinedKeplerElements(p, e, inclination, Ω, ω, nu)
      } else {
        val cos_u = min(max(n * r / (nmag * rmag), -1.0), 1.0)
        val u = Radians(if (r.z < 0) 2 * Pi - acos(cos_u) else acos(cos_u))
        new CircularInclinedKeplerElements(p, inclination, Ω, u)
      }
    } else {
      if (e != 0.0) {
        val cos_ω_true = min(max(ē.x / e, -1.0), 1.0)
        val ω_true = Radians(if (ē.y < 0) 2 * Pi - acos(cos_ω_true) else acos(cos_ω_true))
        new EllipticalEquatorialKeplerElements(p, e, nu, ω_true)
      } else {
        val cos_true_longitude = min(max(r.x / rmag, -1.0), 1.0)
        val true_longitude = Radians(if (r.y < 0) 2 * Pi - acos(cos_true_longitude) else acos(cos_true_longitude))
        new CircularEquatorialKeplerElements(p, true_longitude)
      }
    }
  }

  def intercept(targetState: TARGET_STATE, dt: Double, shortPathTrajectory: Boolean = true): InterceptSolution = targetState match {
    case INITIAL_STATE(targetInitialState) => intercept(targetInitialState, targetInitialState.state(dt), dt, shortPathTrajectory)
    case FINAL_STATE(targetFinalState) => intercept(targetFinalState.state(-dt), targetFinalState, dt, shortPathTrajectory)
  }

  def intercept(targetInitialState: ECIStateVector, targetFinalState: ECIStateVector, dt: Double, shortPathTrajectory: Boolean): InterceptSolution =
    Functions.Lambert(position, targetFinalState.position, dt, shortPathTrajectory) match {
      case Some(velocities) => {
        val transferState = ECIStateVector(position, velocities._1)
        val interceptorFinalState = transferState.state(dt)
        val initialDeltaV = velocities._1 - velocity
        //Interceptor final state is wrong!
        new InterceptSolution(this, targetInitialState, dt, initialDeltaV, transferState,
          interceptorFinalState, targetFinalState)
      }
      case None => throw new Exception("No intercept solution found.")
    }

  def rendezvous(targetInitialState: ECIStateVector, targetFinalState: ECIStateVector, dt: Double, shortPathTrajectory: Boolean): RendezvousSolution =
    Functions.Lambert(position, targetFinalState.position, dt, shortPathTrajectory) match {
      case Some(velocities) => {
        val initialTransferState = ECIStateVector(position, velocities._1)
        val finalTransferState = ECIStateVector(targetFinalState.position, velocities._2)
        val initialDeltaV = velocities._1 - velocity
        val finalDeltaV = targetFinalState.velocity - velocities._2
        new RendezvousSolution(this, targetInitialState, dt, initialDeltaV,
          initialTransferState, finalDeltaV,
          finalTransferState, targetFinalState)
      }
      case None => throw new Exception("No intercept solution found.")
    }

  def hitEarth(dt: Double) = if (toKeplerElements.suborbital) {
    val futureState = state(dt)
    if ((position * velocity < 0.0) && (futureState.position * futureState.velocity > 0.0)) {
      toKeplerElements.suborbital
    } else false
  } else false

  lazy val specificAngularMomentum = position ⨯ velocity
  def h = specificAngularMomentum
  lazy val specificMechanicalEnergy = velocity * velocity * 0.5 - WGS84.GM / position.mag

  def beta(t : JD) = Angle.acos(Sol.direction(t) * h.normalized).complement

  def inclinationOnlyOptions(t: JD) = {
    val kps = toKeplerElements
    val an = kps.ascendingNode
    val dn = kps.descendingNode
    val an_dt = timeOfFlight(an.position)
    val dn_dt = timeOfFlight(dn.position)
    SortedMap((t plusSeconds an_dt) -> an, (t plusSeconds dn_dt) -> dn)
  }

  def timeOfFlight(eci : Vector3) = Functions.timeOfFlight(this, eci)

  def timeOfFlight(that: ECIStateVector): Double = timeOfFlight(that.position)

  override def state = this

  def state(dt: Double): ECIStateVector = keplers.state(dt)
  
  def phi = atan2(position.y, sqrt(position.x * position.x + position.y * position.y))
  def beta = asin(inclination.cos * cos(phi))
  def inclinationRange = {
    val cphi = cos(phi)
    (acos(cphi), acos(-cphi))
  }

  /**
   * x_lo
   * x_hi
   */
  def perturb(d: Double) = {
    val ps = Jacobians.perturb(position, d) map { ECIStateVector(_, velocity) }
    val vs = Jacobians.perturb(velocity, d) map { ECIStateVector(position, _) }
    ps ++ vs
  }

  def jacobian[T](wrt: T, delta: Double)(f: (scala.collection.immutable.Vector[ECIStateVector], Double) => Array[Array[Double]]): Array[Array[Double]] = {
    f(perturb(delta), delta)
  }

  /**
   * Delta-V required to change the semi-major axis to a. This is usually done
   * when the flight path angle is low so that other parameters are unaffected.
   */
  def axisDeltaV(a: Double) =  DVUtils.axisDeltaV(this, a)
  def rangeDeltaV(r: Double) = DVUtils.rangeDeltaV(this, r)

  def circularizingDeltaV = DVUtils.circularizingDeltaV(this)

  /**
   * Must be performed at a node crossing
   */
  def inclineDeltaV(di: Angle) = di.rot(position.normalized) * velocity - velocity

  def inclineDeltaMag(di: Angle) = velocity.mag * (2 * di.half.sin * toKeplerElements.flightPathAngle.cos)

  def deltaV(dv: Vector3) = ECIStateVector(position, velocity + dv)
  def deltaP(dp: Vector3) = ECIStateVector(position + dp, velocity)

  def extrapolate(dt: Double) = position + velocity * dt
}
