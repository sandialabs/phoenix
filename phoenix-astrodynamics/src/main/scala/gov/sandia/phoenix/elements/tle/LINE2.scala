package gov.sandia.phoenix.elements.tle

object LINE2 extends TleLine {
  val pattern = "2 NNNNN NNN.NNNN NNN.NNNN NNNNNNN NNN.NNNN NNN.NNNN NN.NNNNNNNNNNNNNN"

  /**A regular expression pattern corresponding to: 2 NNNNN NNN.NNNN NNN.NNNN NNNNNNN NNN.NNNN NNN.NNNN NN.NNNNNNNNNNNNNN*/
  val regex = """(\d .{5}+ [\d\.\s+-]{8}+ [\d\.\s+-]{8}+ [\d\s+-]{7}+ [\d\.\s+-]{8}+ [\d\.\s+-]{8}+ [\s\d]{2}+\.\d{8}+[\s\d]{5}+\d?)\s*""".r

  def unapply(s: String) = s match {
    case regex(s) => Some(s)
    case _ => None
  }

  val fields = Vector(LINE_2_FIELD, SPACE_01_FIELD, SATELLITE_FIELD, SPACE_07_FIELD, INCLINATION_FIELD,
    SPACE_16_FIELD, RIGHT_ASCENSION_FIELD, SPACE_25_FIELD, ECCENTRICITY_FIELD, SPACE_33_FIELD,
    ARGUMENT_OF_PERIGEE_FIELD, SPACE_42_FIELD, MEAN_ANOMALY_FIELD, SPACE_51_FIELD, MEAN_MOTION_FIELD,
    REVOLUTION_NUMBER_FIELD, CHECKSUM_FIELD)
}