package gov.sandia.phoenix.geometry

object Orientations {
  //Front Face
  val R00 = Axes(X_AXIS, Y_AXIS, Z_AXIS)
  val R01 = Axes(Y_AXIS, -X_AXIS, Z_AXIS)
  val R02 = Axes(-X_AXIS, -Y_AXIS, Z_AXIS)
  val R03 = Axes(-Y_AXIS, X_AXIS, Z_AXIS)

  //Right Face
  val R10 = Axes(-Z_AXIS, Y_AXIS, X_AXIS)
  val R11 = Axes(Y_AXIS, Z_AXIS, X_AXIS)
  val R12 = Axes(Z_AXIS, -Y_AXIS, X_AXIS)
  val R13 = Axes(-Y_AXIS, -Z_AXIS, X_AXIS)

  //Top Face
  val R20 = Axes(X_AXIS, -Z_AXIS, Y_AXIS)
  val R21 = Axes(-Z_AXIS, -X_AXIS, Y_AXIS)
  val R22 = Axes(-X_AXIS, Z_AXIS, Y_AXIS)
  val R23 = Axes(Z_AXIS, X_AXIS, Y_AXIS)

  //Back Face
  val R30 = Axes(-X_AXIS, Y_AXIS, -Z_AXIS)
  val R31 = Axes(-Y_AXIS, -X_AXIS, -Z_AXIS)
  val R32 = Axes(X_AXIS, -Y_AXIS, -Z_AXIS)
  val R33 = Axes(Y_AXIS, X_AXIS, -Z_AXIS)

  //Left Face
  val R40 = Axes(Z_AXIS, Y_AXIS, -X_AXIS)
  val R41 = Axes(-Y_AXIS, Z_AXIS, -X_AXIS)
  val R42 = Axes(-Z_AXIS, -Y_AXIS, -X_AXIS)
  val R43 = Axes(Y_AXIS, -Z_AXIS, -X_AXIS)

  //Bottom Face
  val R50 = Axes(-X_AXIS, -Z_AXIS, -Y_AXIS)
  val R51 = Axes(Z_AXIS, -X_AXIS, -Y_AXIS)
  val R52 = Axes(X_AXIS, Z_AXIS, -Y_AXIS)
  val R53 = Axes(-Z_AXIS, X_AXIS, -Y_AXIS)

  val rotations = Array(R00, R01, R02, R03, R10, R11, R12, R13, R20, R21, R22, R23, R30, R31, R32, R33, R40, R41, R42, R43, R50, R51, R52, R53)

  def apply(i : Int) = rotations(i)
  def getInstance = this
}
