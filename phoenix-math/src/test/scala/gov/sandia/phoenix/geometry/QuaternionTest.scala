package gov.sandia.phoenix.geometry

import org.scalatest.FunSuite
import scala.math._

class QuaternionTest extends FunSuite {
  test("identity") {
    val q = IDENTITY_QUATERNION
    val m = RotationMatrix.identity
    assert(q.toMatrix == m)
  }
      
  test("concatenation order") {
    val A = Degrees(90.0).rx
    val B = Degrees(90.0).ry
    val C = Degrees(-90.0).rz
    val D = A * B * C
        
    val x = Vector3(0, 0, -1)
    val y = Vector3(0, 1, 0)
    val z = Vector3(1, 0, 0)
    val ans = Axes(x, y, z)
    assert(D close ans)
        
    val E = IDENTITY_QUATERNION * A * B * C
    assert(E == D)
  }
      
  test("local to global rotation") {
    val A = Degrees(90.0).rx
    val B = Degrees(90.0).ry
    val C = Degrees(-90.0).rz
    var D = A * B * C
        
    val r = Vector3(1, 1, 1)
    var rt = D * r
    val ans = Vector3(1, 1, -1)
    assert(rt close ans)

    D = A.mul(B).mul(C)
    rt = D * r
    assert(rt close ans)
  }
      
  test("global to local rotation 1") {
    val A = Degrees(90.0).rx
    val B = Degrees(90.0).ry
    val C = Degrees(-90.0).rz
    val D = A * B * C
        
    val r = Vector3(1, 1, -1)
    val rt = !D * r
    val ans = Vector3(1, 1, 1)
    assert(rt close ans)
  }
      
  test("global to local rotation 2") {
    val A = Degrees(90.0).rx
    val B = Degrees(90.0).ry
    val C = Degrees(-90.0).rz
    val D = !C * !B * !A
        
    val r = Vector3(1, 1, -1)
    val rt = D * r
    val ans = Vector3(1, 1, 1)
    assert(rt close ans)
  }
      
  test("non-rotating quaternion") {
    val axis = Vector3(3, 5, 1)
    val r = AxisAngle(axis.normalized, Angle.ZERO).toQuaternion
    val rx = Angle.ZERO.rx
    val ry = Angle.ZERO.ry
    val rz = Angle.ZERO.rz
    assert(r close rx)
    assert(r close ry)
    assert(r close rz)
    assert(rx close ry)
    assert(ry close rz)
    assert(rz close rx)
  }
      
  test("Yaw-Pitch-Roll to Quaternion") {
    val yaw = Degrees(120)
    val pitch = Degrees(30)
    val roll = Degrees(-145)
    val q = YPR(yaw, pitch, roll).toQuaternion
    val ans = Quaternion(0.359, Vector3(-0.39321, 0.83672, 0.12813))
        
    assert(abs(q.s - ans.s) < 1E-6)
    assert(abs(q.v.dist(ans.v)) < 1E-5)
  }
      
  test("Quaternion to Yaw-Pitch-Roll") {
    val yaw = Degrees(120)
    val pitch = Degrees(30)
    val roll = Degrees(-145)
    val q = YPR(yaw, pitch, roll).toQuaternion
    val ans = Quaternion(0.359, Vector3(-0.39321, 0.83672, 0.12813)).toYPR
    assert((yaw - ans.yaw).abs < 0.001)
    assert((pitch - ans.pitch).abs < 0.001)
    assert((roll - ans.roll).abs < 0.001)
  }
      
  test("Yaw-Pitch-Roll to Quaternion (Zenith)") {
    //Backwards and upside down
    val roll = Degrees(-180)
    val pitch = Degrees(0)
    val yaw = Degrees(-180)
    val A = YPR(yaw, pitch, roll).toQuaternion
    val ans = Quaternion(0, Vector3(0, 1, 0))
    assert(A close ans)
  }
      
  test("Euler to Quaternion") {
    val A = Degrees(135)
    val B = Degrees(135)
    val C = Degrees(-90)
    val q = EulerZXZ(A, B, C).toQuaternion
    val ans = Quaternion(0.353553, Vector3(-0.353553, 0.853553, 0.146447))
    assert(abs(q.s - ans.s) < 1E-6)
    assert(abs(q.v.dist(ans.v)) < 1E-6)
  }

  test("Quaternion to Euler") {
    val A = Degrees(135)
    val B = Degrees(135)
    val C = Degrees(-90)
    val q = EulerZXZ(A, B, C).toQuaternion
    val ans = Quaternion(0.353553, Vector3(-0.353553, 0.853553, 0.146447)).toEulerZXZ
    assert((A - ans.phi).abs < 0.001)
    assert((B - ans.theta).abs < 0.001)
    assert((C - ans.psi).abs < 0.001)
  }
      
  test("Terminal Quaternion->YPR asin - This is a degenerate case") {
    val q = YPR(Degrees(45), Degrees(-90), Degrees(45)).toQuaternion
    val ypr = q.toYPR
    assert(ypr.pitch.degrees != Double.NaN)
  }
      
  test("Euler to Quaternion (Zenith)") {
    val A = Angle.D180
    val B = Angle.D180
    val C = Angle.ZERO
    val q = EulerZXZ(A, B, C).toQuaternion
    val ans = Quaternion(0, Vector3(0, 1, 0))
    assert(q close ans)
  }

  test("Az-El to Quaternion (Zenith)") {
    val azimuth = 0
    val elevation = -90
    val q = AzEl(azimuth, elevation).toQuaternion
    val ans = Quaternion(0, Vector3(0, 1, 0))

    assert(q close ans)
  }
      
  test("equals") {
    var a = Quaternion(1, Vector3(0, 0, 0))
    var b = Quaternion(-1, Vector3(0, 0, 0))
    assert(a close b)
        
    a = Quaternion(0, Vector3(1, 0, 0))
    b = Quaternion(0, Vector3(-1, 0, 0))
    assert(a close b)
        
    a = Quaternion(0, Vector3(0, 1, 0))
    b = Quaternion(0, Vector3(0, -1, 0))
    assert(a close b)
        
    a = Quaternion(0, Vector3(0, 0, 1))
    b = Quaternion(0, Vector3(0, 0, -1))
    assert(a close b)
        
    a = Degrees(45).rx
    b = Degrees(44).rx
    assert(a != b)
    b = Degrees(45).ry
    assert(a != b)
  }
      
  test("unit") {
    val q = YPR(Angle.D45, Angle.ZERO, Angle.ZERO).toQuaternion
    assert(q.mag == 1)
  }
      
  test("rotate into") {
    val a = Vector3(1, 0, 0)
    var b = Vector3(1, 1, 0).normalized
    var q = a.rotInto(b)
    var p = Vector3(1, 0, 0)
    var r = q * p
    assert(r close b)
        
    b = a
    q = a.rotInto(b)
    p = Vector3(1, 0, 0)
    r = q * p
    assert(r close b)
  }
}