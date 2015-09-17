package gov.sandia.phoenix.uom

sealed abstract class Time {
  def doubleValue : Double
  def build(newValue : Double) : Time
  def unit = build(1)

  def ms = Milliseconds(seconds.doubleValue * 1000)
  def seconds : Time
  def minutes : Time = Minutes(seconds.doubleValue / 60.0)
  def hours : Time = Hours(minutes.doubleValue / 60.0)
  def days : Time = Days(hours.doubleValue / 24.0)
  def weeks : Time = Weeks(days.doubleValue / 7.0)

  def * (x : Double) = build(doubleValue * x)
  def / (that : Time) = this.seconds.doubleValue / that.seconds.doubleValue
  def + (that : Time) = Seconds(this.seconds.doubleValue + that.seconds.doubleValue)

  def min(that : Time) = if(this.seconds.doubleValue < that.seconds.doubleValue) this else that
  def max(that : Time) = if(this.seconds.doubleValue > that.seconds.doubleValue) this else that
}

case class Milliseconds(doubleValue : Double) extends Time {
  final override def ms = this
  final override def seconds = Seconds(doubleValue / 1000.0)
  def build(newValue : Double) = Milliseconds(newValue)
}
case class Seconds(doubleValue : Double) extends Time {
  final override def seconds = this
  def build(newValue : Double) = Seconds(newValue)
}
case class Minutes(doubleValue : Double) extends Time {
  final override def minutes = this
  final override def seconds = Seconds(doubleValue * 60)
  def build(newValue : Double) = Minutes(newValue)
}
case class Hours(doubleValue : Double) extends Time {
  final override def hours = this
  final override def seconds = Seconds(doubleValue * 60 * 60)
  def build(newValue : Double) = Hours(newValue)
}
case class Days(doubleValue : Double) extends Time {
  final override def days = this
  final override def seconds = Seconds(doubleValue * 24 * 60 * 60)
  def build(newValue : Double) = Days(newValue)
}
case class Weeks(doubleValue : Double) extends Time {
  final override def weeks = this
  final override def seconds = Seconds(doubleValue * 7 * 24 * 60 * 60)
  def build(newValue : Double) = Weeks(newValue)
}

object Second extends Seconds(1)

