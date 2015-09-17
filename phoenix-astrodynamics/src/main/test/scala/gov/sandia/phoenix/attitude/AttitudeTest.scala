package gov.sandia.phoenix.attitude

import gov.sandia.phoenix.attitudes._
import gov.sandia.phoenix.elements.tle.TLE
import gov.sandia.phoenix.geometry._
import gov.sandia.phoenix.propagators.sgp4.SGP4
import gov.sandia.phoenix.time.TimeBuilder
import org.scalatest.FunSuite

class AttitudeTest extends FunSuite {

  val l0 = "MOLNIYA 1-93"
  val l1 = "1 28163U 04005A   14235.56278564 -.00000137  00000-0  00000+0 0  3846"
  val l2 = "2 28163  64.3157  66.4838 7374357 245.6152  20.1015  2.00611325 77018"
  val tle = TLE(Some(l0), l1, l2)
  val p = SGP4(tle)

  val epoch = TimeBuilder(2014, 8, 25)

  test("UTN") {
    val state = p.unsafe_state(epoch)
    assert(UTN(state) close Axes.ZY(-state.position, state.velocity))
  }

  test("RSW") {
    val state = p.unsafe_state(epoch)
    assert(RSW(state) close Axes.XY(state.position, state.velocity))
  }

  test("NTW") {
    val state = p.unsafe_state(epoch)
    assert(NTW(state) close Axes.YX(state.velocity, state.position))
  }

  test("RPY") {
    val state = p.unsafe_state(epoch)
    assert(RPY(state) close Axes.XZ(state.velocity, -state.position))
  }

  test("SEZ") {
    val state = p.unsafe_state(epoch)
    assert(SEZ(state) close Axes.ZX(state.position, -Z_AXIS))
  }

  test("North Pole") {
    val state = Geodetic(0, 90, 0)
    val a = Axes(X_AXIS, Y_AXIS, Z_AXIS)
    val b = Axes.ZX(state.toECEF, -Z_AXIS)
    assert(a close b)
  }

  test("South Pole") {
    val state = Geodetic(0, -90, 0)
    val a = Axes(-X_AXIS, Y_AXIS, -Z_AXIS)
    val b = Axes.ZX(state.toECEF, -Z_AXIS)
    assert(a close b)
  }
}
