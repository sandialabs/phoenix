/*
 * Copyright (c) 2016 Sandia Corporation. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by the
 * terms of this license.
 * You must not remove this notice, or any other, from this software.
 *
 * Contributors:
 * - Mark Bastian: Original author.
 * - See Git logs.
 */

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
