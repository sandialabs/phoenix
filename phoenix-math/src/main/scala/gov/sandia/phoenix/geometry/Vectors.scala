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

package gov.sandia.phoenix.geometry

import gov.sandia.phoenix.constants.WGS84

object ORIGIN extends Vector3(0, 0, 0)
object X_AXIS extends Vector3(1, 0, 0)
object Y_AXIS extends Vector3(0, 1, 0)
object Z_AXIS extends Vector3(0, 0, 1)
object I_HAT extends Vector3(1, 0, 0)
object J_HAT extends Vector3(0, 1, 0)
object K_HAT extends Vector3(0, 0, 1)
object NORTH_POLE extends Vector3(0, 0, WGS84.R_EQ_M)
object SOUTH_POLE extends Vector3(0, 0, -WGS84.R_EQ_M)