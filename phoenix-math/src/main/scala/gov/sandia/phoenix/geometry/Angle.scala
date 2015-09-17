package gov.sandia.phoenix.geometry

/**
 *
 */
trait Angle {
  def radians : Double
  def degrees : Double
  def sin = math.sin(radians)
  def cos = math.cos(radians)
  def tan = math.tan(radians)
  def sign = math.signum(radians)
  def abs = math.abs(radians)
  def complement : Angle
  def supplement : Angle
  def explement : Angle
  def constrainUnsigned : Angle
  def constrainSigned : Angle
  def unary_- : Angle
  override def toString = "\u2220" + degrees + "\u00B0"

  def + (that : Angle) : Angle
  def - (that : Angle) : Angle
  def * (x : Double) : Angle

  def rx = rot(X_AXIS)
  def ry = rot(Y_AXIS)
  def rz = rot(Z_AXIS)
  def rot(axis : Vector3) = AxisAngle(axis, this).toQuaternion

  def % (that : Angle) : Angle

  def half : Angle
  def absDiff(that : Angle) = (this - that).abs

  def / (that : Angle) = this.radians / that.radians

  def plus(that : Angle) = this + that
  def minus(that : Angle) = this - that
  def times(x : Double) = this * x
  def div(that : Angle) = this / that
  def mod(that : Angle) = this % that
  def negated = -this
}

case class Radians(radians : Double) extends Angle {
  def unary_- = Radians(-radians)
  def * (x : Double) = Radians(radians * x)
  def degrees = radians.toDegrees
  def complement = Angle.HalfPi - this
  def supplement = Angle.Pi - this
  def explement = Angle.TwoPi - this
  def half = Radians(0.5 * radians)
  def + (that : Angle) = Radians(this.radians + that.radians)
  def - (that : Angle) = Radians(this.radians - that.radians)
  def % (that : Angle) = Radians(this.radians % that.radians)
  def constrainUnsigned = Radians(Angle.rads0To2Pi(radians))
  def constrainSigned = Radians(Angle.radsMinusPiToPi(radians))
}

case class Degrees(degrees : Double) extends Angle {
  def unary_- = Degrees(-degrees)
  def * (x : Double) = Degrees(degrees * x)
  def radians = degrees.toRadians
  def complement = Angle.D90 - this
  def supplement = Angle.D180 - this
  def explement = Angle.D360 - this
  def half = Degrees(0.5 * degrees)
  def + (that : Angle) = Degrees(this.degrees + that.degrees)
  def - (that : Angle) = Degrees(this.degrees - that.degrees)
  def % (that : Angle) = Degrees(this.degrees % that.degrees)
  def constrainUnsigned = Degrees(Angle.degs0To360(degrees))
  def constrainSigned = Degrees(Angle.degsMinus180To180(degrees))
}

object Angle {
  def radians(θ : Double) = Radians(θ)
  def degrees(θ : Double) = Degrees(θ)

  def rads0To2Pi(θ : Double) : Double = θ match {
    case _ if θ < 0 => rads0To2Pi(θ + 2.0 * math.Pi)
    case _ if θ >= 2.0 * math.Pi => rads0To2Pi(θ - 2.0 * math.Pi)
    case _ => θ
  }

  def radsMinusPiToPi(θ : Double) : Double = θ match {
    case _ if θ < -math.Pi => radsMinusPiToPi(θ + 2.0 * math.Pi)
    case _ if θ >= math.Pi => radsMinusPiToPi(θ - 2.0 * math.Pi)
    case _ => θ
  }

  def degs0To360(θ : Double) : Double = θ match {
    case _ if θ < 0 => degs0To360(θ + 360)
    case _ if θ >= 360 => degs0To360(θ - 360)
    case _ => θ
  }

  def degsMinus180To180(θ : Double) : Double = θ match {
    case _ if θ < -180 => degsMinus180To180(θ + 360)
    case _ if θ >= 180 => degsMinus180To180(θ - 360)
    case _ => θ
  }

  def acos(θ : Double) = Radians(math.acos(θ))
  def asin(θ : Double) = Radians(math.asin(θ))
  def atan(θ : Double) = Radians(math.atan(θ))
  def atan2(y : Double, x : Double) = Radians(math.atan2(y, x))

  val ZERO = Radians(0.0)
  val Pi = Radians(math.Pi)
  val TwoPi = Radians(math.Pi * 2.0)
  val HalfPi = Radians(math.Pi * 0.5)
  val NaN = Radians(Double.NaN)

  val D45 = Degrees(45)
  val D90 = Degrees(90.0)
  val D180 = Degrees(180.0)
  val D360 = Degrees(360.0)
}
