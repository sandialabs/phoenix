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

package gov.sandia.phoenix.conf

import java.net.URI
import java.util.Properties
import java.util.logging.Logger

import scala.util.{Success, Try}

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 *
 */
object SGConf extends Properties {
  val logger = Logger.getLogger(getClass.getName)
  val DTED_LOCATION = "dted.location"
  val DTED_CACHE_SIZE = "dted.cache_size"
  val EOP_FILE = "eop.file"
  val HARRIS_PRIESTER_DENSITY_MODEL_FILE = "harris-priester.data"

  setProperty(DTED_CACHE_SIZE, "50")
  setProperty(DTED_LOCATION, System.getProperty("user.home"))
  setProperty("PHOENIX.home", System.getProperty("user.home") + "/PHOENIX")
  setProperty(EOP_FILE, getProperty("PHOENIX.home") + "/EOP/EOP.dat.all")
  setProperty(HARRIS_PRIESTER_DENSITY_MODEL_FILE, getProperty("PHOENIX.home") + "/data/harrispriester.txt")

  def getURI(key : String) = getProperty(key) match {
    case null => None
    case uriString => Try(URI.create(uriString)) match {
      case Success(uri) => Some(uri)
      case _ => None
    }
  }
}