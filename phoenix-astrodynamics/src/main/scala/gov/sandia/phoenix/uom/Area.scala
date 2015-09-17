package gov.sandia.phoenix.uom

/**
 * To create a new area type you must override the method defining your unit (i.e. def unit = this)
 * and a method to convert your unit to a base unit (unless it is a base unit). Note that all unit
 * conversions should be done in a base unit type to prevent infinite recursion.
 */
sealed abstract class Area {
  def doubleValue: Double

  def build(newValue: Double): Area

  def scale(s: Double) = build(doubleValue * s)

  def squareMeters: Area

  def squareKilometers: Area = SquareKilometers(squareMeters.doubleValue / 1.0E6)

}

case class SquareMeters(doubleValue : Double) extends Area {
  final override def squareMeters = this

  def build(newValue : Double) = SquareMeters(newValue)
}

case class SquareKilometers(doubleValue : Double) extends Area {
  final override def squareKilometers = this

  final override def squareMeters = SquareMeters(doubleValue * 1.0E6)

  def build(newValue : Double) = SquareKilometers(newValue)
}