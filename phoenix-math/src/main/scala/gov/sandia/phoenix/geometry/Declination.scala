package gov.sandia.phoenix.geometry

object Declination {
  def fromDegrees(decimalDegrees : Double) = {
    val degrees = decimalDegrees.toInt
    val dmin = (decimalDegrees - degrees) * 60
    val min = dmin.toInt
    val sec = (dmin - min) * 60
    new Declination(degrees, min, sec)
  }
  def fromRadians(radians : Double) = fromDegrees(radians.toDegrees)
}

case class Declination(degrees : Int, minutes : Int, seconds : Double) {
  def decimalDegrees = degrees + (minutes + seconds / 60.0) / 60.0
  def radians = decimalDegrees.toRadians
  
  override def toString = (if(degrees >= 0) "+" else "") +
  degrees + "\u00B0" + minutes + "'" + seconds + "\""
}
