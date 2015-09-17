package gov.sandia.phoenix.sp

import java.net._
import java.util.logging.{Level, Logger}

/**
 * The only class that uses this is HPDModel. Consider just adding what is needed there are removing this.
 *
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
object URIUtil {
  val logger = Logger.getLogger(getClass.getName)
  val httprgx = """http:.+""".r
  val filergx = """file:.+""".r
  def apply(s : String) : URI = try { s match {
    case httprgx() => URI.create(s)
    case filergx() => URI.create(s)
    case _ => new java.io.File(s).toURI
  }} catch {
    case e : Exception => logger.log(Level.SEVERE, "Could not open URI.", e); e.printStackTrace(); null
  }

  def toURI(s : String) = this(s)
  def toURL(s : String) = toURI(s).toURL
  def toInputStream(s : String) = toURL(s).openStream
  def toSource(s : String) = scala.io.Source.fromInputStream(toInputStream(s))
  def toLineList(s : String) = toSource(s).getLines().toList

  def toLineList(uri : URI) : List[String] = try {
    scala.io.Source.fromInputStream(uri.toURL.openStream).getLines().toList
  } catch {
    case e : Exception => logger.log(Level.SEVERE, "Could not construct line list.", e); e.printStackTrace(); null
  }
}
