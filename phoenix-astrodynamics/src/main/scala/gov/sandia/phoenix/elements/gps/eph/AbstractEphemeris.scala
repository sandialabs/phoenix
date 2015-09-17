package gov.sandia.phoenix.elements.gps.eph


/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
abstract class AbstractEphemeris extends Ephemeris
{
  def semiMajorAxis = squareRootOfSemiMajorAxis * squareRootOfSemiMajorAxis
}