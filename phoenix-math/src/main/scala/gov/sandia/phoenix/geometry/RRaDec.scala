package gov.sandia.phoenix.geometry

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
case class RRaDec(range : Double, rightAscension : Double, declination : Double) {
  def pretty = "Range, Ra, Dec : " + range + " m, " + rightAscension + "\u00B0" + declination + "\u00B0"
}