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