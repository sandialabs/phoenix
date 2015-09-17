package gov.sandia.phoenix.elements.tle


import gov.sandia.phoenix.time.TimeBuilder

object LINE1 extends TleLine {
  val pattern = "1 NNNNNC NNNNNAAA NNNNN.NNNNNNNN +.NNNNNNNN +NNNNN-N +NNNNN-N N NNNNN"

  /** A regular expression pattern corresponding to: 1 NNNNNC NNNNNAAA NNNNN.NNNNNNNN +.NNNNNNNN +NNNNN-N +NNNNN-N N NNNNN */
  val regex = """(\d .{5}+.{1}+ [\d\s]{2}+[\d\s]{3}+[\w\s]{3}+ \d{2}+[\s\d\.]{12}+ [\s+-0]\.[\d]{8}+ [\s+-][\d\.\s+-]{5}+[+-]\d [\s+-][\d\.\s+-]{5}+[+-]\d [\d\s]{1}+ [\d\s]{4}+\d?)\s*""".r

  def unapply(s: String) = s match {
    case regex(x) => Some(x)
    case _ => None
  }

  val fields = Vector(LINE_1_FIELD, SPACE_01_FIELD, SATELLITE_FIELD, CLASSIFICATION_FIELD, SPACE_08_FIELD,
    INTERNATIONAL_DESIGNATOR_YEAR_FIELD, INTERNATIONAL_DESIGNATOR_LAUNCH_NUMBER_FIELD, INTERNATIONAL_DESIGNATOR_LAUNCH_PIECE_FIELD,
    SPACE_17_FIELD, EPOCH_YEAR_FIELD, EPOCH_DAY_FIELD, SPACE_32_FIELD, MEAN_MOTION_FIRST_DERIVATIVE_FIELD, SPACE_43_FIELD,
    MEAN_MOTION_SECOND_DERIVATIVE_FIELD, SPACE_52_FIELD, BSTAR_DRAG_FIELD, SPACE_61_FIELD, EPHEMERIS_TYPE_FIELD,
    SPACE_63_FIELD, ELEMENT_NUMBER_FIELD, CHECKSUM_FIELD)

  def matches(line : String) = fields forall { _.apply(line).returnCode match {
      case TLE_EXTRACTION_ERROR(message) => false
      case _ => true
    }
  }

  def epoch(line : String) = EPOCH_YEAR_FIELD(line).value flatMap { year =>
    EPOCH_DAY_FIELD(line).value flatMap { dayOfYear =>
      Some(TimeBuilder(year, 1, 0) plusDays dayOfYear)
    }
  }
}