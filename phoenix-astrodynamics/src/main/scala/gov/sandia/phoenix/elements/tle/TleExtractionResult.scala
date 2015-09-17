package gov.sandia.phoenix.elements.tle

sealed abstract class TLE_EXTRACTION_RESULT {
  def message : String
}
case object TLE_EXTRACTION_SUCCESS extends TLE_EXTRACTION_RESULT { def message = "Success" }
case class TLE_EXTRACTION_WARNING(message : String) extends TLE_EXTRACTION_RESULT
case class TLE_EXTRACTION_ERROR(message : String) extends TLE_EXTRACTION_RESULT

case class TleExtractionResult[T](value : Option[T],
                                  raw_string : String,
                                  line : String,
                                  field : TleField[T],
                                  returnCode : TLE_EXTRACTION_RESULT) {
  def infoString = returnCode match {
    case TLE_EXTRACTION_ERROR(message) => message
    case TLE_EXTRACTION_WARNING(message) => valueString + " (" + message + ")"
    case _ => valueString
  }

  private final def epochString = if(field == EPOCH_YEAR_FIELD || field == EPOCH_DAY_FIELD) {
    LINE1.epoch(line) map { t => " (" + t.toString() + ")" } getOrElse "" } else ""

  def valueString : String = (value map { _.toString } getOrElse "") + epochString
}