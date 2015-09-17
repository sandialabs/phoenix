package gov.sandia.phoenix.uom

  sealed abstract class Power {
    def doubleValue: Double

    def build(newValue: Double): Power

    def scale(s: Double) = build(doubleValue * s)

    def watts: Power

    def kilowatts: Power = Kilowatts(watts.doubleValue / 1000)
  }

  case class Watts(doubleValue: Double) extends Power {

    final override def watts = this

    def build(newValue: Double) = Watts(newValue)
  }

  case class Kilowatts(doubleValue: Double) extends Power {

    final override def kilowatts = this

    final override def watts = Watts(doubleValue * 1000)

    def build(newValue: Double) = Kilowatts(newValue)
  }
