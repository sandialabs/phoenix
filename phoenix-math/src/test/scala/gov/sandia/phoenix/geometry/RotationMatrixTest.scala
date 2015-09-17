package gov.sandia.phoenix.geometry



import org.scalatest.FunSuite

import scala.math._


class RotationMatrixTest extends FunSuite
{
  test("concatenation order")
  {
    val A = RotationMatrix.rotX(toRadians(90.0))
    val B = RotationMatrix.rotY(toRadians(90.0))
    val C = RotationMatrix.rotZ(toRadians(-90.0))
    val D = A * B * C
        
    val x = new Vector3(0, 0, -1)
    val y = new Vector3(0, 1, 0)
    val z = new Vector3(1, 0, 0)
    val ans = RotationMatrix.axes(x, y, z)
    assert(D == ans)
        
    val E = RotationMatrix.identity * A * B * C
    assert(E == D)
  }
      
  test("local to global rotation")
  {
    val A = RotationMatrix.rotX(toRadians(90.0))
    val B = RotationMatrix.rotY(toRadians(90.0))
    val C = RotationMatrix.rotZ(toRadians(-90.0))
    var D = A * B * C
        
    val r = new Vector3(1, 1, 1)
    var rt = D * r
    val ans = new Vector3(1, 1, -1)
    assert(rt close ans)

    D = A.mul(B).mul(C)
    rt = D * r
    assert(rt close ans)
  }
      
  test("global to local rotation 1")
  {
    val A = RotationMatrix.rotX(toRadians(90.0))
    val B = RotationMatrix.rotY(toRadians(90.0))
    val C = RotationMatrix.rotZ(toRadians(-90.0))
    val D = A * B * C
        
    val r = new Vector3(1, 1, -1)
    val rt = !D * r
    val ans = new Vector3(1, 1, 1)
    assert(rt close ans)
  }
      
  test("global to local rotation 2")
  {
    val A = RotationMatrix.rotX(toRadians(90.0))
    val B = RotationMatrix.rotY(toRadians(90.0))
    val C = RotationMatrix.rotZ(toRadians(-90.0))
    val D = !C * !B * !A
        
    val r = new Vector3(1, 1, -1)
    val rt = D * r
    val ans = new Vector3(1, 1, 1)
    assert(rt close ans)
  }
      
  test("Yaw-Pitch-Roll to RotationMatrix")
  {
    val yaw = Degrees(120)
    val pitch = Degrees(30)
    val roll = Degrees(-145)
    val q = YPR(yaw, pitch, roll).toQuaternion.toMatrix
    val ans = Quaternion(0.359, Vector3(-0.39321, 0.83672, 0.12813)).toMatrix
        
    assert(q == ans)
  }
      
  test("RotationMatrix to Yaw-Pitch-Roll")
  {
    val yaw = Degrees(120)
    val pitch = Degrees(30)
    val roll = Degrees(-145)
    val q = YPR(yaw, pitch, roll).toQuaternion
    val ans = Quaternion(0.359, Vector3(-0.39321, 0.83672, 0.12813)).toMatrix.toYPR
    assert((yaw - ans.yaw).abs < 0.001)
    assert((pitch - ans.pitch).abs < 0.001)
    assert((roll - ans.roll).abs < 0.001)
  }
      
  test("Yaw-Pitch-Roll to RotationMatrix (Zenith)")
  {
    //Backwards and upside down
    val roll = Degrees(-180)
    val pitch = Angle.ZERO
    val yaw = Degrees(-180)
    val A = YPR(yaw, pitch, roll).toQuaternion.toMatrix
    val ans = new Quaternion(0, new Vector3(0, 1, 0)).toMatrix
    assert(A == ans)
  }
      
  test("Euler to RotationMatrix")
  {
    val A = Degrees(135)
    val B = Degrees(135)
    val C = Degrees(-90)
    val q = EulerZXZ(A, B, C).toQuaternion.toMatrix
    val ans = new Quaternion(0.353553, new Vector3(-0.353553, 0.853553, 0.146447)).toMatrix
    assert(q == ans)
  }
      
  test("RotationMatrix to Euler")
  {
    val A = Degrees(135)
    val B = Degrees(135)
    val C = Degrees(-90)
    val q = EulerZXZ(A, B, C).toQuaternion
    val ans = new Quaternion(0.353553, new Vector3(-0.353553, 0.853553, 0.146447)).toMatrix.toEulerZXZ
    assert((A - ans.phi).abs < 0.001)
    assert((B - ans.theta).abs < 0.001)
    assert((C - ans.psi).abs < 0.001)
  }
      
  test("Euler to RotationMatrix (Zenith)")
  {
    val A = Degrees(180)
    val B = Degrees(180)
    val C = Degrees(0)
    val q = EulerZXZ(A, B, C).toQuaternion.toMatrix
    val ans = new Quaternion(0, new Vector3(0, 1, 0)).toMatrix
    assert(q == ans)
  }
      
  test("Az-El to RotationMatrix")
  {
    val azimuth = 45
    val elevation = -45
    val q = RotationMatrix.AzEl(azimuth, elevation)
    val ans = new Quaternion(0.353553, new Vector3(-0.353553, 0.853553, 0.146447)).toMatrix
    assert(q == ans)
  }
      
  test("Az-El to RotationMatrix (Zenith)")
  {
    val azimuth = 0
    val elevation = -90
    val q = RotationMatrix.AzEl(azimuth, elevation)
    val ans = new Quaternion(0, new Vector3(0, 1, 0)).toMatrix
    assert(q == ans)
  }
      
  test("rX")
  {
    val m = RotationMatrix.rotX(toRadians(90.0))
    val v = m * Y_AXIS
    assert(abs(v.x) < pow(2, -32))
    assert(abs(v.y) < pow(2, -32))
    assert(v.z == 1.0)
  }
      
  test("rY")
  {
    val m = RotationMatrix.rotY(toRadians(90.0))
    val v = m * X_AXIS
    assert(abs(v.x) < pow(2, -32))
    assert(abs(v.y) < pow(2, -32))
    assert(v.z == -1.0)
  }
      
  test("rZ")
  {
    val m = RotationMatrix.rotZ(toRadians(90.0))
    val v = m * X_AXIS
    assert(abs(v.x) < pow(2, -32))
    assert(v.y == 1.0)
    assert(abs(v.z) < pow(2, -32))
  }
      
  test("X(90)*Y(90)*<0,0,1>=<1,0,0>")
  {
    val rx = RotationMatrix.rotX(toRadians(90.0))
    val ry = RotationMatrix.rotY(toRadians(90.0))
    val v = rx * ry * Z_AXIS
    assert(v.x == 1.0)
    assert(abs(v.y) < pow(2, -32))
    assert(abs(v.z) < pow(2, -32))
  }
}