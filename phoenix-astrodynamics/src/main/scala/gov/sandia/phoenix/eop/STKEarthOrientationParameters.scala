package gov.sandia.phoenix.eop

import java.net._
import java.util.logging.Logger
import gov.sandia.phoenix.conf.SGConf
import gov.sandia.phoenix.sp.URIUtil
import gov.sandia.phoenix.time._

import scala.collection.immutable._
import scala.language.postfixOps

/**
 * Files obtained from ftp.stk.com/pub/DynamicEarthData/
 * username: anonymous
 * password: anonymous
 * <p>
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
class STKEarthOrientationParameters(val MJD : Int, x : Double,
                                    val xError : Double, y : Double,
                                    val yError : Double, dUT1 : Double,
                                    val dUT1Error : Double)
extends EOPEntry(dUT1, x, y, TAI(new MJD(MJD).toJulianDate.value.doubleValue()).toInt)

class EOPLocator {
  def locate = SGConf.getURI("eop.file") getOrElse {
    val uri = EOPLocator.this.getClass.getResource("EOP.dat.all").toURI
    val logger = Logger.getLogger(getClass.getName)
    logger.warning("Embedded EOP.dat.all file is being used. It may be stale.")
    uri
  }
}

object STKEarthOrientationParameters
extends EarthOrientationParameters(STKEOPReader(new EOPLocator locate))

object STKEOPReader {
  val logger = Logger.getLogger(getClass.getName)

  final def apply(uri : URI) = read(URIUtil.toLineList(uri), Map.empty[Int, EOPEntry])

  private final def read(s : List[String], m : Map[Int, EOPEntry]) : Map[Int, EOPEntry] = s match {
    case Nil => m
    case STKEOPReader(eop) :: tail => read(tail, m+(eop.MJD->eop))
    case head :: tail => read(tail, m)
  }

  private final def unapply(s : String) = {
    val values = s.trim.replaceAll("\\s+", ",").split(",")
    if(values.length != 7) 
      None
    else
      Some(new STKEarthOrientationParameters(values(0).toDouble.toInt,
                                             values(1).toDouble, values(2).toDouble, values(3).toDouble,
                                             values(4).toDouble, values(5).toDouble, values(6).toDouble))
  }
}