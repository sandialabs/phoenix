package gov.sandia.phoenix.uom


sealed abstract class PowerPerArea {

  def doubleValue : Double

  def wattsPerSquareMeter: PowerPerArea

  def kilowattsPerSquareMeter: PowerPerArea = WattsPerSquareMeter(wattsPerSquareMeter.doubleValue / 1000)
}

case class WattsPerSquareMeter(doubleValue : Double) extends PowerPerArea {

  final override def wattsPerSquareMeter = this

  def this(power: Power, area : Area)  =
    this(power.watts.doubleValue / area.squareMeters.doubleValue)
}

case class KilowattsPerSquareMeter(doubleValue : Double) extends PowerPerArea {

  final override def kilowattsPerSquareMeter = this

  final override def wattsPerSquareMeter = WattsPerSquareMeter(doubleValue * 1000)
}

case class Irradiance(doubleValue: Double) extends PowerPerArea {

  def this(power: Power,   area : Area)  =
    this(power.watts.doubleValue / area.squareMeters.doubleValue)

  final override def wattsPerSquareMeter = this

  final override def kilowattsPerSquareMeter = KilowattsPerSquareMeter(doubleValue)
}

/**
 * Example of alternate class for Irradiance. A companion object is defined for the
 * auxiliary constructor in order to keep the same syntax (i.e. no 'new' keyword) when
 * instantiating Irradiance objects from doubleValue.
 */
case class AlternateIrradiance(power: Power, area: Area) extends PowerPerArea {

  def doubleValue = power.watts.doubleValue / area.squareMeters.doubleValue

  final override def wattsPerSquareMeter =
    WattsPerSquareMeter(power.watts.doubleValue / area.squareMeters.doubleValue)

  final override def kilowattsPerSquareMeter = KilowattsPerSquareMeter(doubleValue)
}

/*
object AlternateIrradiance {
  def apply(doubleValue: Double) = AlternateIrradiance(Watts(doubleValue), SquareMeters(1))
}
*/

/**
 * This class has implementation identical to Irradiance but it represents a different
 * physical concept.
 */
case class RadiantExitance(doubleValue: Double) extends PowerPerArea {

  def this(power: Power,   area : Area)  =
    this(power.watts.doubleValue / area.squareMeters.doubleValue)

  final override def wattsPerSquareMeter = this

  final override def kilowattsPerSquareMeter = KilowattsPerSquareMeter(doubleValue)
}

/**
 * This class has implementation identical to Irradiance but it represents a different
 * physical concept.
 */
case class RadiantEmittance(doubleValue: Double) extends PowerPerArea {

  def this(power: Power,   area : Area)  =
    this(power.watts.doubleValue / area.squareMeters.doubleValue)

  final override def wattsPerSquareMeter = this

  final override def kilowattsPerSquareMeter = KilowattsPerSquareMeter(doubleValue)
}

