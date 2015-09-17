package gov.sandia.phoenix.geometry

trait IInterpolable {
  def interpolate(x : Double) : Vector3
  def sample(pointsPerSegment : Int, inclusive : Boolean = false) = (inclusive match {
    case true => (0 to pointsPerSegment) map { i => interpolate(i.toDouble / pointsPerSegment) }
    case false => (0 until pointsPerSegment) map { i => interpolate(i.toDouble / pointsPerSegment) }
  }).toArray
}
