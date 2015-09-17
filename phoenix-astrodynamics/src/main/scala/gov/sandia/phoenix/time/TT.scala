package gov.sandia.phoenix.time

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
object TT {
  def apply(jd_utc : Double) = TAI(jd_utc) + 32.184
  def apply(t_utc : JD) = TAI(t_utc) + 32.184
}
