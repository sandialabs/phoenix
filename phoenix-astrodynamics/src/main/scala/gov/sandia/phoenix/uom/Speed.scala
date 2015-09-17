package gov.sandia.phoenix.uom

sealed abstract class Speed {
  def doubleValue : Double

  def feetPerSecond : Speed = FeetPerSecond(Meters(metersPerSecond.doubleValue).feet.doubleValue)
  def metersPerSecond : Speed = MetersPerSecond(Feet(feetPerSecond.doubleValue).meters.doubleValue)

  def knots : Speed = Knots(kilometersPerHour.doubleValue / 1.852)
  def milesPerHour : Speed = MilesPerHour(feetPerSecond.doubleValue * 3600 / 5280)
  def kilometersPerHour : Speed = KilometersPerHour(metersPerSecond.doubleValue * 3.6)

  def distance(time : Time) : Distance = Meters(metersPerSecond.doubleValue * time.seconds.doubleValue)
}

case class FeetPerSecond(doubleValue : Double) extends Speed {
  final override def feetPerSecond = this
}

case class MetersPerSecond(doubleValue : Double) extends Speed {
  final override def metersPerSecond = this
}

case class MilesPerHour(doubleValue : Double) extends Speed {
  final override def milesPerHour = this
  final override def feetPerSecond = FeetPerSecond(doubleValue * 5280 / 3600)
}

case class KilometersPerHour(doubleValue : Double) extends Speed {
  final override def kilometersPerHour = this
  final override def metersPerSecond = MetersPerSecond(doubleValue / 3.6)
}

case class Knots(doubleValue : Double) extends Speed {
  final override def knots = this
  final override def metersPerSecond = MetersPerSecond(doubleValue * 1852.0 / 3600.0)
}

case class UserDefinedSpeed(distance : Distance, time : Time) extends Speed {
  def doubleValue = distance.doubleValue / time.doubleValue
  final override def metersPerSecond = MetersPerSecond(distance.meters.doubleValue / time.seconds.doubleValue)
}