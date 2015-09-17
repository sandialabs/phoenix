package gov.sandia.phoenix.geometry



/**
 * Class for storing azimuth, elevation, and range values.
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class AzElR(azimuth : Double, elevation : Double, range : Double) {
  require (elevation >= -90 && elevation <= 90)

  def toSEZ = Degrees(-azimuth).rz * Degrees(elevation).ry * Vector3(-range, 0, 0)

  def csv = azimuth + ", " + elevation + ", " + range
}