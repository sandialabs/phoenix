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

package gov.sandia.phoenix.dted

import java.io._
import java.text.DecimalFormat
import java.util.logging.{Level, Logger}

import gov.sandia.phoenix.collections.LRUMap
import gov.sandia.phoenix.conf.SGConf
import gov.sandia.phoenix.constants.WGS84
import gov.sandia.phoenix.geometry._
import gov.sandia.phoenix.numerics._

import scala.math._
import scala.util.matching._

/**
 * DTED instances are essentially the loaded contents of a single DTED data file,
 * which corresponds to a tile of elevation data.
 * 
 * See http://earth-info.nga.mil/publications/specs/printed/89020B/89020B.pdf
 * for a file spec.
 */
class DTED(val origin : Geodetic, val SW : Geodetic,
           val SE : Geodetic, val NE : Geodetic,
           val NW : Geodetic, val numLongitudeLines : Int,
           val numLatitudeLines : Int, val longitudeInterval : Float,
           val latitudeInterval : Float, val elevations : Array[Float],
           val minElevation : Float, val maxElevation : Float) {
  override def toString = "DTED\n\tSW = " + SW.toString + "\n\tNE = " + NE.toString + 
  "\n\tdimensions = " + numLongitudeLines + " x " + numLatitudeLines +
  "\n\tspacing = " + longitudeInterval + " x " + latitudeInterval +
  "\n\tmin/max elevation = " + minElevation + "/" + maxElevation + " m"
  
  def elevation(lon : Double, latitude : Double) = {
    val longitude = DTED.wrapLongitude(lon)
    require(longitude >= SW.longitude && longitude <= NE.longitude && 
            latitude >= SW.latitude && latitude <= NE.latitude)
    val x = ((longitude - SW.longitude) / (NE.longitude - SW.longitude)) * (numLongitudeLines - 1)
    val y = ((latitude - SW.latitude) / (NE.latitude - SW.latitude)) * (numLatitudeLines - 1)

    val i = x.intValue
    val j = y.intValue
    val ll = h(i, j)
    val lr = h(i + 1, j)
    val ur = h(i + 1, j + 1)
    val ul = h(i, j + 1)
    
    val lo = (x - i) * (lr - ll) + ll
    val hi = (x - i) * (ur - ul) + ul
    (y - j) * (hi - lo) + lo
  }
  
  private def h(i : Int, j : Int) = elevations(j * numLongitudeLines + i)
}

object DTED {
  private lazy val cache = new LRUMap[Int, Option[DTED]](SGConf.getProperty(SGConf.DTED_CACHE_SIZE).toInt)
  private val empties = new scala.collection.mutable.HashSet[Int]
  private val UHLPattern = """.{4}(.{3})(.{2})(.{2})(.{1})(.{3})(.{2})(.{2})(.{1})(\d{4})(\d{4}).{4}.{3}.{12}(\d{4})(\d{4})\p{ASCII}{25}""".r
  private val DDMMSSdSH = """(\d{2})(\d{2})([.\d]{4})([NS]{1})"""
  private val DDDMMSSdSH = """(\d{3})(\d{2})([.\d]{4})([EW]{1})"""
  private val DDMMSSH = """(\d{2})(\d{2})(\d{2})([NS]{1})"""
  private val DDDMMSSH = """(\d{3})(\d{2})(\d{2})([EW]{1})"""
  private val COORD19 = """([.\dNSEW]{19})"""
  private val COORD15 = """([\dNSEW]{15})"""
  private val DSIPattern = (""".{3}.{1}.{137}.{3}(.{5}).{36}""" + COORD19 + COORD15 + COORD15 + COORD15 + COORD15 + """.*""").r
  private val OriginLonLatPattern = (DDMMSSdSH + DDDMMSSdSH).r
  private val LonLatPattern = (DDMMSSH + DDDMMSSH).r
  private val minHeight = -420 //The shores of the Dead Sea
  private val maxHeight = 1E4 //Cap out a bit above Everest
  private val stepMeters = 100.0 //Good for dted level 1
  private val triDigit = new DecimalFormat("000")
  private val twoDigit = new DecimalFormat("00")
  private val logger = Logger.getLogger(getClass.getName)
    
  private def wrapLongitude(lon : Double) : Double = if(lon < -180) wrapLongitude(lon + 360) else if(lon >= 180) wrapLongitude(lon - 360) else lon
  
  /**
   * Compute the elevation at the intersection of the given ray with the Earth.
   */
  def intersect(ray : Ray) : Option[Geodetic] = ray->WGS84.ellipsoid match {
    case null => None
    case intersection => {
        val sez = (ray -> WGS84.ellipsoid).toGeodetic.toSEZFrame
        val tSez = sez.toSEZ(ray.origin)
        val oSez = if(tSez.z > maxHeight) tSez * (maxHeight / tSez.z) else tSez
        val dSez = oSez / oSez.z * minHeight
        val hypot = sqrt(oSez.x * oSez.x + oSez.y * oSez.y)
        val steps = max(ceil(hypot / stepMeters), 1)
        val ls = new LineSegment(sez * oSez, sez * dSez)
    
        def ele(t : Double) = {
          val geo = ls.interpolate(t).toGeodetic
          geo.elevation - DTED.elevation(geo.longitude, geo.latitude)
        }
    
        for(step <- (0 to steps.intValue) find ((i) => ele(i / steps) < 0.0);
            ra = Numerics.falsepos(ele, (step - 1) / steps, step / steps))
              yield ls.interpolate(ra).toGeodetic
      }
  }
  
  /**
   * Compute the elevation using the DTED data or return 0 if there is no DTED
   * data for this location.
   */
  def elevation(longitude : Double, latitude : Double) : Double = this(longitude, latitude)
  
  def apply(longitude : Double, latitude : Double) : Double = {
    val lon = DTED.wrapLongitude(longitude)
    DTED.tileFor(floor(lon).intValue, floor(latitude).intValue) match {
      case Some(dted) => dted.elevation(lon, latitude)
      case None => 0.0
    }
  }
  
  def keyfcn(lon : Int, lat : Int) = ((lon + 180) << 8) & 0xFFFFFF00 | (lat + 90) & 0x000000FF
  
  def tileFor(longitude : Int, latitude : Int) = {
    val key = keyfcn(longitude, latitude)
    if(empties(key)) None
    else if(cache.containsKey(key)) cache(key)
    else load(longitude, latitude) match {
      case None => empties+=key; None
      case dted => cache.put(key, dted); dted
    }  
  }

  final def using[I <: InputStream, A](is : I)(f : I => Option[A]) = try {
    f(is)
  } catch {
    case ex : Exception => logger.log(Level.SEVERE, "Exception in using block.", ex); None
    case _ : Throwable => logger.log(Level.SEVERE, "Unhandled Case!"); None
  } finally is.close

  def load(longitude : Int, latitude : Int) : Option[DTED] = {
    val file = new File(locationFor(longitude, latitude))
    if(file.exists) using(new DataInputStream(new FileInputStream(file))) {
      logger.info("Loading DTED file " + file.getPath + ".")
      is => Some(read(is)) 
    } else {
      logger.info("DTED loader could not find file \"" + file + "\" for use " +
                  "with longitude = " + longitude + " latitude = " + latitude + 
                  ". 0 will be returned for all elevation calculations.")
      None
    }
  }
  
  def locationFor(longitude : Int, latitude : Int) = {
    val ns = (if(latitude < 0) "s" else "n") + (twoDigit format abs(latitude))
    val ew = (if(longitude < 0) "w" else "e") + (triDigit format abs(longitude))
    val dtedLocation = SGConf.getProperty(SGConf.DTED_LOCATION)
    dtedLocation + "/DTED/" + ew + "/" + ns + ".dt1"  
  }
  
  private def read(is : DataInput) : DTED = {
    //Read and parse the User Header Label (UHL)
    val UHL = new Array[Byte](80)
    is.readFully(UHL)
    val UHLPattern(originLonDegrees, originLonMinutes, originLonSeconds, 
                   originLonHemisphere, originLatDegrees, originLatMinutes, originLatSeconds, 
                   originLatHemisphere, lonDataInterval, latDataInterval, numLonLines, numLatLines) = new String(UHL)
    val secondsOfLongitude = lonDataInterval.toInt * 0.1f
    val secondsOfLatitude = latDataInterval.toInt * 0.1f
    val numLongitudeLines = numLonLines.toInt
    val numLatitudeLines = numLatLines.toInt
    
    //Read and parse the Data Set Identification (DSI) Record
    val DSI = new Array[Byte](648)
    is.readFully(DSI)
    val DSIPattern(datum, o, sw, nw, ne, se) = new String(DSI)
    val origin = parseLonLat(o, OriginLonLatPattern)
    val SW = parseLonLat(sw, LonLatPattern)
    val NW = parseLonLat(nw, LonLatPattern)
    val NE = parseLonLat(ne, LonLatPattern)
    val SE = parseLonLat(se, LonLatPattern)
            
    val ACC = new Array[Byte](2700)
    is.readFully(ACC)
    val accString = new String(ACC)
    
    val data = new Array[Float](numLongitudeLines * numLatitudeLines)
    val bytes = new Array[Byte](numLatitudeLines * 2 + 8)
    val shorts = java.nio.ByteBuffer.wrap(bytes)

    var minElevation = Float.MaxValue
    var maxElevation = -minElevation
      
    for(i <- 0 until numLongitudeLines) {
      is.readFully(bytes)
      
      val sentinelAndDataBlockCount = shorts.getInt
      val sentinel = (sentinelAndDataBlockCount >> (3 * java.lang.Byte.SIZE)) & 0xFF
      val dataBlockCount = sentinelAndDataBlockCount & 0x00FFFFFF
      val longitudeCount = shorts.getShort
      val latitudeCount = shorts.getShort
      
      for(j <- 0 until numLatitudeLines) {
        data(j * numLongitudeLines + i) = { 
          val s = shorts.getShort
          if(s < 0) -(s & 0x00007FFF).floatValue else s.floatValue
        }
        minElevation = min(minElevation, data(j * numLongitudeLines + i))
        maxElevation = max(maxElevation, data(j * numLongitudeLines + i))
      }
      
      shorts.rewind

      //Everest is 8848m
      assert(maxElevation < 8900, "This DTED has a max elevation greater than the highest point on the Earth!")
      //Dead Sea is -418
      assert(minElevation > -420, "This DTED has a min elevation lower than the lowest point on the Earth!")
      //Compute checksum
      assert((is.readInt /: bytes)((i, b) => i - (b & 0xFF)) == 0)
    }
    
    new DTED(origin, SW, SE, NE, NW, numLongitudeLines, numLatitudeLines, 
             secondsOfLongitude, secondsOfLatitude, data, minElevation, maxElevation)
  }
  
  private def parseLonLat(ll : String, regex : Regex) = {
    val regex(latDD, latMM, latSS, latH, lonDD, lonMM, lonSS, lonH) = ll
    val lon = (lonDD.toFloat + (lonMM.toFloat + lonSS.toFloat / 60) / 60) * (if (lonH == "E") 1 else -1)
    val lat = (latDD.toFloat + (latMM.toFloat + latSS.toFloat / 60) / 60) * (if (latH == "N") 1 else -1)
    Geodetic(lon, lat, 0)
  }
}
