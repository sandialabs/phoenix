package gov.sandia.phoenix.elements.tle

object LINE0 {
  def unapply(s: String) = {
    val trimmed = s.trim
    if(trimmed.length == 0 || trimmed.startsWith("#")) None else Some(trimmed)
  }
}