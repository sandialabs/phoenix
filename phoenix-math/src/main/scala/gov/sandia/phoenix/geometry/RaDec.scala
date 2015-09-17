package gov.sandia.phoenix.geometry

/**
 * Right Ascension and Declination, both in degrees
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class RaDec(rightAscension : RightAscension, declination : Declination) {
  override def toString = rightAscension + ", " + declination

  def mid(that : RaDec) = 
    new RaDec(RightAscension.fromDegrees((this.rightAscension.degrees + that.rightAscension.degrees) * 0.5), 
        Declination.fromDegrees((this.declination.decimalDegrees + that.declination.decimalDegrees) * 0.5))
  
  def toECI = Radians(rightAscension.radians).rz * Radians(-declination.radians).ry * X_AXIS
}