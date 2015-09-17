package gov.sandia.phoenix.sp

import gov.sandia.phoenix.constants._
import gov.sandia.phoenix.geometry._

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
object TwoBody {
  def acceleration(r : Vector3, s : Vector3, GM : Double) = {
    val rm = r.mag
    val sm = s.mag
    val smr = s - r
    val smrm = smr.mag
    val t2 = if(sm != 0.0) s / (sm * sm * sm) else s
    (smr / (smrm * smrm * smrm) - t2) * GM
  }
  
  def earthGravity(r : Vector3) = acceleration(r, new Vector3(0, 0, 0), WGS84.GM)
}
