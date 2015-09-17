package gov.sandia.phoenix.geometry

import gov.sandia.phoenix.math._

import scala.math._

case class AzElRange(minimum : AzEl, maximum : AzEl) {
  def minAzimuth = minimum.azimuth
  def getMinAzimuth = minAzimuth
  def maxAzimuth = maximum.azimuth
  def getMaxAzimuth = maxAzimuth
  def minElevation = minimum.elevation
  def getMinElevation = minElevation
  def maxElevation = maximum.elevation
  def getMaxElevation = maxElevation

  def parametricEl(x : Double) = min(90, max(-90, minElevation + x * (maxElevation - minElevation)))

  def parametricAz(x : Double) = minAzimuth + x * (maxAzimuth - minAzimuth)

  def parametricAzEl(i : Double, j : Double) = {
    val azimuth = parametricAz(i)
    val elevation = constrainElevation(minimum.elevation + j * (maximum.elevation - minimum.elevation))
    new AzEl(azimuth, elevation)
  }

  def parametricAzElR(i : Double, j : Double, r : Double) = {
    val azimuth = parametricAz(i)
    val elevation = constrainElevation(minimum.elevation + j * (maximum.elevation - minimum.elevation))
    new AzElR(azimuth, elevation, r)
  }

  def paramaterizeAz(az : Double) = constrain0to360(az - minAzimuth) / constrain0to360(maxAzimuth - minAzimuth)

  def containsElevation(e : Double) = e >= minElevation && e <= maxElevation
  def containsAzimuth(a : Double) = {
    val p = paramaterizeAz(a)
    p >= 0.0 && p <= 1.0
  }

  def contains(azel : AzEl) = containsAzimuth(azel.azimuth) && containsElevation(azel.elevation)

//  def top(near : Double, far : Double) = {
//    val dim = 200
//    val r  = { t : Double => t * (far - near) + near }
//
//    Array.tabulate[AzElR](dim){ i =>  parametricAzElR(i / (dim - 1.0), 1.0, near) }++
//      Array.tabulate[AzElR](dim){ i => parametricAzElR(1.0, 1.0, r(i / (dim - 1.0))) }++
//      Array.tabulate[AzElR](dim){ i =>  parametricAzElR(1.0 - i / (dim - 1.0), 1.0, far) }++
//      Array.tabulate[AzElR](dim){ i => parametricAzElR(0.0, 1.0, r(1.0 - i / (dim - 1.0))) }
//  }
//
//  def bottom(near : Double, far : Double) = {
//    val dim = 200
//    val r  = { t : Double => t * (far - near) + near }
//
//    Array.tabulate[AzElR](dim){ i =>  parametricAzElR(i / (dim - 1.0), 0.0, near) }++
//      Array.tabulate[AzElR](dim){ i => parametricAzElR(1.0, 0.0, r(i / (dim - 1.0))) }++
//      Array.tabulate[AzElR](dim){ i =>  parametricAzElR(1.0 - i / (dim - 1.0), 0.0, far) }++
//      Array.tabulate[AzElR](dim){ i => parametricAzElR(0.0, 0.0, r(1.0 - i / (dim - 1.0))) }
//  }
//
//  def left(near : Double, far : Double) = {
//    val dim = 200
//    val r  = { t : Double => t * (far - near) + near }
//
//    Array.tabulate[AzElR](dim){ i => parametricAzElR(0.0, r(i / (dim - 1.0)), near) }++
//    Array.tabulate[AzElR](dim){ i => parametricAzElR(0.0, 1.0, r(i / (dim - 1.0))) }++
//    Array.tabulate[AzElR](dim){ i => parametricAzElR(0.0, r(1.0 - i / (dim - 1.0)), far) }++
//    Array.tabulate[AzElR](dim){ i => parametricAzElR(0.0, 0.0, r(1.0 - i / (dim - 1.0))) }
//  }

  def front(segments : Int) = {
    val delta = 1.0 / segments
    //Do something like this...
    //Doesn't quite work as shown by the current example
    val bottom = if(minElevation == -90.0) Array.empty[AzEl] else Array.tabulate(segments){ i => parametricAzEl(i * delta, 0.0) }
    val upside = if(maxAzimuth - minAzimuth == 360) Array.empty[AzEl] else Array.tabulate(segments){ i => parametricAzEl(1.0, i * delta) }
    val downside = if(maxAzimuth - minAzimuth == 360) Array.empty[AzEl] else Array.tabulate(segments){ i => parametricAzEl(0.0, 1.0 - i * delta) }
    val top = if(maxElevation == 90.0) Array.empty[AzEl] else Array.tabulate(segments){ i => parametricAzEl(1.0 - i * delta, 1.0) }
    bottom++upside++top++downside
  }

  def crossSection(elevation : Double, near : Double, far : Double) = {
    val dim = 200
    val r  = { t : Double => t * (far - near) + near }

    Array.tabulate[AzElR](dim){ i => new AzElR(parametricAz(i / (dim - 1.0)), elevation, near) }++
    Array.tabulate[AzElR](dim){ i => new AzElR(parametricAz(1.0), elevation, r(i / (dim - 1.0))) }++
    Array.tabulate[AzElR](dim){ i => new AzElR(parametricAz(1.0 - i / (dim - 1.0)), elevation, far) }++
      Array.tabulate[AzElR](dim){ i => new AzElR(parametricAz(0.0), elevation, r(1.0 - i / (dim - 1.0))) }
  }

  def footprint(near : Double, far : Double) = {
    val dim = 200
    val r  = { t : Double => t * (far - near) + near }

    Array.tabulate[AzElR](dim){ i => parametricAzElR(i / (dim - 1.0), 1.0, near) }++ //across near high
      Array.tabulate[AzElR](dim){ i => parametricAzElR(1.0, 1.0 - i / (dim - 1.0), near) }++ //down
      Array.tabulate[AzElR](dim){ i => parametricAzElR(1.0, 0.0, r(i / (dim - 1.0))) }++ //out low
    Array.tabulate[AzElR](dim){ i => parametricAzElR(1.0 - i / (dim - 1.0), 0.0, far) }++ //Across low
      Array.tabulate[AzElR](dim){ i => parametricAzElR(0.0, 0.0, r(1.0 - i / (dim - 1.0))) }++ //Return low
      Array.tabulate[AzElR](dim){ i => parametricAzElR(0.0, i / (dim - 1.0), near) }
  }

  def constrainAzimuth(θ : Double) = min(maxAzimuth, max(minAzimuth, θ))
  def constrainElevation(θ : Double) = min(maxElevation, max(minElevation, θ))

  def constrain(azel : AzEl) : AzEl = new AzEl(constrainAzimuth(azel.azimuth), constrainElevation(azel.elevation))

  override def toString = minimum + " - " + maximum
}
