package gov.sandia.phoenix.propagators.sgp4

/**
 * Note that sin and cos values for these parameters are hand entered rather than calling the respective math functions.
 * This is done to ensure consistency with legacy codes that precompute these values.
 */
case class SolarConstants(jdepoch : Double) extends LSConstants {
  val ω = 281.2208
  val sω = -0.98088458
  val cω = 0.1945905
  val Ω = 0.0
  val sΩ = 0.0
  val cΩ = 1.0
  val I = ε
  val sI = 0.39785416
  val cI = 0.91744867
  val eccentricity = 0.01675
  //Solar mean motion, in radians per minute
  val meanMotion = 1.19459e-5
  val C = 2.9864797e-6 //Solar perturbation coefficient (Radians per minute)

  //Used in Appendix A.E.
  //See Appendix B.B.5 Update for Long-Period Periodic Effects of Lunar and Solar Gravity (MX =MOX + nX * dt)
  def M(tsince : Double) = SGP4Util.mod2pi(6.2565837 + 0.017201977 * ((jdepoch - 2433281.5) + 18261.5)) + meanMotion * tsince
}
