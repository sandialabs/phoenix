package gov.sandia.phoenix.elements.tle

import scala.util.matching.Regex

object TleLine {
  def apply(line : String) = line match {
    case l1 if line.startsWith("1 ") && line.length >= TLEConstants.LINE_LENGTH => Some(LINE1)
    case l2 if line.startsWith("2 ") && line.length >= TLEConstants.LINE_LENGTH => Some(LINE2)
    case _ => None
  }

  def extractValues(line : String) = this(line) map { _.extractValues(line) }
  def valueAt(line : String, pos : Int) = this(line) flatMap { _.valueAt(line, pos) }
}

abstract class TleLine {
  val pattern : String
  val regex : Regex
  val fields : IndexedSeq[TleField[_ <: Any]]

  private lazy val field_by_index = (0 until TLEConstants.LINE_LENGTH) map { index =>
    fields find { field => index >= field.startIndex && index < field.endIndex }
  }

  def fieldAt(pos : Int) = if(pos < TLEConstants.LINE_LENGTH && pos >= 0) field_by_index(pos) else None
  def extractValues(line : String) = fields map { field => field.apply(line) }
  def valueAt(line : String, pos : Int) = fieldAt(pos) map { _.apply(line) }
}
