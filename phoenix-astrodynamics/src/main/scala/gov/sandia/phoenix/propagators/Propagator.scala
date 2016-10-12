/*
 * Copyright (c) 2016 Sandia Corporation. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by the
 * terms of this license.
 * You must not remove this notice, or any other, from this software.
 *
 * Contributors:
 * - Mark Bastian: Original author.
 * - See Git logs.
 */

package gov.sandia.phoenix.propagators

import gov.sandia.phoenix.elements.sv.ECIStateVector
import gov.sandia.phoenix.geometry.{ORIGIN, Vector3}
import gov.sandia.phoenix.time.JD

/**
 * A propagator is an object that is used to compute the location/state of a
 * satellite.
 *
 * Some typical propagators are:
 *
 * Kepler Elements: Idealized two-body elements using Kepler's Laws. Note that
 * without an epoch to reference the elements in time, you cannot propagate keps.
 *
 * ECI State Vector: Position and Velocity representation of a state. Can be
 * exchanged with Kepler elements for same solution two body propagation. Can
 * also be propagated using a force model for high accuracy propagation. Also
 * requires an epoch.
 *
 * TLE: Two Line Elements - A string based format that has some extra terms for
 * things like drag and better than two-body geometry. Sometimes has a line 0
 * for the name of the satellite, but that is optional. Epoch is included.
 *
 * Almanac/Ephemeris: GPS specific propagators that are very precise. Almanac is
 * pretty good and is valid for months. Ephemeris is very precise and is only
 * good for a few hours.
 *
 * SP3: Historical GPS observations that can be interpolated using Lagrange
 * polynomials for extremely precise state calculations. SP3s are published days
 * or weeks after the actual orbit, so are historical in nature.
 */
trait Propagator {
  def state(t : JD) : Option[ECIStateVector]

  def position(t : JD) = state(t) map { _.position }
  def velocity(t : JD) = state(t) map { _.velocity }

  def withState[T](t: JD, default: T)(f: ECIStateVector => T) = state(t).fold(default)(f)
  def withPos[T](t: JD, default: T)(f: Vector3 => T) = position(t).fold(default)(f)
  def withVel[T](t: JD, default: T)(f: Vector3 => T) = velocity(t).fold(default)(f)

  def unsafe_state(t : JD) = withState(t, ECIStateVector(ORIGIN, ORIGIN)){ sv => sv }

  def o : Vector3 = ORIGIN

  def unsafe_position(t : JD) = withPos(t, o){ pos => pos }
  def unsafe_velocity(t : JD) = withVel(t, o){ vel => vel }
}