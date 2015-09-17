package gov.sandia.phoenix.propagators.sgp4

sealed abstract class SatRecMethod
case object NearSpace extends SatRecMethod
case object DeepSpace extends SatRecMethod
