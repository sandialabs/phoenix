package gov.sandia.phoenix.elements.tle

object TLEFilters {
  val geo = (tle : TLE) => tle.meanMotion * TLEUtils.MINUTES_PER_REVOLUTION match {
    case m if m >= 0.9 && m <= 1.1 => true
    case _ => false
  }

  val debris = (tle : TLE) => tle.name contains " DEB"
  val rocket_body = (tle : TLE) => tle.name contains " R/B"

  def inclination(lo : Double, hi : Double) = { (tle : TLE) => tle.inclination <= lo && tle.inclination >= hi }
}
