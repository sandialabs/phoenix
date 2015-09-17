package gov.sandia.phoenix.solarsystem

import java.io._
import java.util.zip._

import gov.sandia.phoenix._

import scala.io._
import scala.math._

/**
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 */
abstract class GravitationalCoefficients {
  def C(n : Int, m : Int) : Double
  def S(n : Int, m : Int) : Double
  def m : Int
  def n : Int
}

object GravitationalCoefficients {
  private final def kd(i : Int, j : Int) = if(i == j) 1 else 0
  private final def f(n : Int, m : Int) = (BigDecimal(1, java.math.MathContext.DECIMAL128) /: (n-m+1 to n+m))(_*BigDecimal(_))
  private final def den(n : Int, m : Int) = (2 - kd(0, m)) * (2 * n + 1)
  final def scale(n : Int, m : Int) = BigMath.sqrt(f(n, m) / den(n, m))
}

object EGM96GravityCoefficients extends GravitationalCoefficients {
  private final def parseCoefficients = {
    val is = getClass.getResourceAsStream("egm96.gcon")
    for(line <- Source.fromInputStream(is).getLines(); s = line.trim.replaceAll("\\s+", ",")) s match {
      case EGM96Line(n, m, cnm, snm, sigmaCnm, sigmaSnm) =>
        val s = GravitationalCoefficients.scale(n, m)
        C(n)(m) = (cnm / s).doubleValue
        S(n)(m) = (snm / s).doubleValue
      case _ => println("Misunderstood line")
    }
  }
  
  def readCoefficients : Unit = {
    val dat = new java.io.File("egm96.gcon.dat")
    if(dat.exists) {
      val is = new DataInputStream(new GZIPInputStream(new FileInputStream(dat)))
      for(i <- 0 until C.length; j <- 0 until C(i).length) C(i)(j) = is.readDouble
      for(i <- 0 until S.length; j <- 0 until S(i).length) S(i)(j) = is.readDouble
      is.close
    } else {
      parseCoefficients
      val os = new DataOutputStream(new GZIPOutputStream(new FileOutputStream(dat)))
      for(i <- 0 until C.length; j <- 0 until C(i).length) os.writeDouble(C(i)(j))
      for(i <- 0 until S.length; j <- 0 until S(i).length) os.writeDouble(S(i)(j))
      os.close
      readCoefficients
    }
  }
  
  object EGM96Line {
    def unapply(s : String) = {
      val x = s split ","
      if(x.length == 6) Some(x(0).toInt, x(1).toInt, 
                             BigDecimal(x(2), java.math.MathContext.DECIMAL128),
                             BigDecimal(x(3), java.math.MathContext.DECIMAL128),
                             x(4).toDouble, x(5).toDouble) else None
    }
  }
  
  private val C = Array.ofDim[Double](361, 361)
  private val S = Array.ofDim[Double](361, 361)
  C(0)(0) = 1
  readCoefficients
  
  final def C(n : Int, m : Int) = C(n)(m)
  final def S(n : Int, m : Int) = S(n)(m)
  final def m = C.length - 1
  final def n = C(0).length - 1
}

object WGS84GravityCoefficients extends GravitationalCoefficients {
  private final def parseCoefficients = {
    val is = getClass.getResourceAsStream("wgs84.gcon")
    val regex = """(.{5})(.{5})(.{15})(.{15})""".r
    for(line <- Source.fromInputStream(is).getLines()) line match {
      case regex(sn, sm, scnm, ssnm) =>
        val n = sn.trim.toInt
        val m = sm.trim.toInt
        val cnm = BigDecimal(scnm.trim, java.math.MathContext.DECIMAL128)
        val snm = BigDecimal(ssnm.trim, java.math.MathContext.DECIMAL128)
        val s = GravitationalCoefficients.scale(n, m)
        C(n)(m) = (cnm / s).doubleValue
        S(n)(m) = (snm / s).doubleValue
      case x => println("Misunderstood line: " + x)
    } 
  }
    
  def readCoefficients : Unit = {
    val dat = new java.io.File("wgs84.gcon.dat")
    if(dat.exists) {
      val is = new DataInputStream(new GZIPInputStream(new FileInputStream(dat)))
      for(i <- 0 until C.length; j <- 0 until C(i).length) C(i)(j) = is.readDouble
      for(i <- 0 until S.length; j <- 0 until S(i).length) S(i)(j) = is.readDouble
      is.close
    } else {
      parseCoefficients
      val os = new DataOutputStream(new GZIPOutputStream(new FileOutputStream(dat)))
      for(i <- 0 until C.length; j <- 0 until C(i).length) os.writeDouble(C(i)(j))
      for(i <- 0 until S.length; j <- 0 until S(i).length) os.writeDouble(S(i)(j))
      os.close
      readCoefficients
    }
  }
    
  private val C = Array.ofDim[Double](181, 181)
  private val S = Array.ofDim[Double](181, 181)
  C(0)(0) = 1
  readCoefficients
  final def C(n : Int, m : Int) = C(n)(m)
  final def S(n : Int, m : Int) = S(n)(m)
  final def m = C.length - 1
  final def n = C(0).length - 1
}

/**
 * http://www.iges.polimi.it/pagine/services/repo/repo_model.asp
 * http://www.agu.org/journals/jb/v101/iB12/96JB01645/
 *
 * @author [[mailto:markbastian@gmail.com Mark Bastian]]
 *
 */
object JGM3GravityCoefficients extends GravitationalCoefficients {
  private final val a6 = """(.{6})"""
  private final val i2 = """([\s\d]{2})"""
  private final val d21 = """([\s\d\.+-E]{21})"""
  private final val d12 = """([\s\d\.+-E]{12})"""
  private final val d4 = """([\s\d\.+-]+)"""
  private final val jgm3Pattern = a6 + i2 + i2 + d21 + d21 + d12 + d12 + d4
  private final val rgx = jgm3Pattern.r

  private val C = Array.ofDim[Double](71, 71)
  private val S = Array.ofDim[Double](71, 71)
  C(0)(0) = 1

  read(Source.fromInputStream(getClass.getResourceAsStream("JGM-3")).getLines().toList)

  def read(s : List[String]) : Unit = s match {
    case rgx(a, sn, sm, scnm, ssnm, x, y, z) :: tail => {
        val n = sn.trim.toInt
        val m = sm.trim.toInt
        val cnm = BigDecimal(scnm.trim, java.math.MathContext.DECIMAL128)
        val snm = BigDecimal(ssnm.trim, java.math.MathContext.DECIMAL128)
        val s = GravitationalCoefficients.scale(n, m)
        C(n)(m) = (cnm / s).doubleValue
        S(n)(m) = (snm / s).doubleValue
        read(tail)
      }
    case head :: tail => read(tail)
    case _ =>
  }

  final def C(n : Int, m : Int) = C(n)(m)
  final def S(n : Int, m : Int) = S(n)(m)
  final def m = C.length - 1
  final def n = C(0).length - 1
}