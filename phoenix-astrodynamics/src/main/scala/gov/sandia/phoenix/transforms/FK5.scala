package gov.sandia.phoenix.transforms

import gov.sandia.phoenix.conf.SGConf
import gov.sandia.phoenix.constants._
import gov.sandia.phoenix.elements.sv.{ECEFStateVector, ECIStateVector, TEMEStateVector, TODStateVector}
import gov.sandia.phoenix.eop.{CelestrakEarthOrientationParameters, STKEarthOrientationParameters}
import gov.sandia.phoenix.geometry._
import gov.sandia.phoenix.time._

import scala.collection.immutable.{Vector => ImmutableArray}
import scala.math._

/**
 * J2000/ECI : ECI = !precession*!nutation*!sidereal*!polar*ECEF
 * 
 * Mean of Date/MOD : precession*ECI = !nutation*!sidereal*!polar*ECEF
 * 
 * True of Date/TOD : nutation*precession*ECI = = nutation*MOD = !sidereal*!polar*ECEF
 * 
 * Pseudo Earth Fixed/PEF: sidereal*nutation*precession*ECI = sidereal*TOD = !polar*ECEF
 * 
 * True Earth Fixed/ITRF/ECEF: polar*sidereal*nutation*precession*ECI = polar*PEF = ECEF
 */
class FK5(val utc : JD, val ut1Offset : Double,
          val deltaAT : Int, val xp : Double,
          val yp : Double, val LOD : Double)
{
  val ut1 = utc.plusSeconds(ut1Offset)
  val tai = utc.plusSeconds(deltaAT)
  val tt = tai.plusSeconds(32.184)
  val ttJulianCenturyJ2000 = tt.toJulianCenturyJ2000
  //Vallado, Equations 3-57
  val zeta = FK5.zeta(ttJulianCenturyJ2000)
  val theta = FK5.theta(ttJulianCenturyJ2000)
  val z = FK5.z(ttJulianCenturyJ2000)

  val w = new Vector3(0, 0, 7.29211514670698e-5 * (1.0 - LOD / Time.SEC_PER_DAY))
  
  //Used for calculating precession, nutation, and sidereal rotations
  val ebar = FK5.ebar(ttJulianCenturyJ2000)
  val omega = FK5.omega(ttJulianCenturyJ2000)
  val l = FK5.l(ttJulianCenturyJ2000)
  val l1 = FK5.l1(ttJulianCenturyJ2000)
  val f = FK5.f(ttJulianCenturyJ2000)
  val d = FK5.d(ttJulianCenturyJ2000)
  val (dPsi, dEpsilon) = FK5.dPsi_dEpsilon(ttJulianCenturyJ2000, l, l1, f, d, omega)
    
  /**
   * Meeus, eq. 20.4
   */
  def equatorialReduction(radec : RaDec) = {
    val ra0 = radec.rightAscension.radians
    val dec0 = radec.declination.radians
    val sinDec = sin(dec0)
    val cosDec = cos(dec0)
    val sinRaZ = sin(ra0 + zeta)
    val cosRaZ = cos(ra0 + zeta)
    val sinTheta = sin(theta)
    val cosTheta = cos(theta)
    val A = cosDec * sinRaZ
    val B = cosTheta * cosDec * cosRaZ - sinTheta * sinDec
    val C = sinTheta * cosDec * cosRaZ + cosTheta * sinDec
    val ra = atan2(A, B) + z
    val dec = asin(C)
    new RaDec(RightAscension.fromRadians(ra), Declination.fromRadians(dec))
  }

  override def toString = {
    "Transformations for " + utc + "\n" +
    "ut1Offset: " + ut1Offset + "\n" +
    "deltaAT: " + deltaAT + "\n" +
    "yp: " + yp + "\n" +
    "xp: " + xp + "\n" +
    "precession: " + precession + "\n" +
    "nutation: " + nutation + "\n" +
    "sidereal: " + sidereal + "\n" +
    "polar: " + polar;
  }
    
  val precession = {
    val sZeta = sin(zeta)
    val cZeta = cos(zeta)
    val sTheta = sin(theta)
    val cTheta = cos(theta)
    val sZ = sin(z)
    val cZ = cos(z)

    //p. 221 of Vallado
    val m = ImmutableArray(cTheta*cZ*cZeta-sZ*sZeta, -sZeta*cTheta*cZ-sZ*cZeta, -sTheta*cZ,
                           sZ*cTheta*cZeta+sZeta*cZ, -sZ*sZeta*cTheta+cZ*cZeta, -sTheta*sZ,
                           sTheta*cZeta            , -sTheta*sZeta            , cTheta)
    new RotationMatrix(m).toQuaternion
  }
  

  
  val nutation = {
    val epsilon = ebar + dEpsilon
    val sE = sin(epsilon)
    val cE = cos(epsilon)
    val sEBar = sin(ebar)
    val cEBar = cos(ebar)
    val sPsi = sin(dPsi)
    val cPsi = cos(dPsi)
    val m = ImmutableArray(cPsi, -sPsi*cEBar, -sPsi*sEBar,
                           sPsi*cE, cE*cPsi*cEBar+sE*sEBar, sEBar*cE*cPsi-sE*cEBar,
                           sE*sPsi, sE*cPsi*cEBar-sEBar*cE, sE*sEBar*cPsi+cE*cEBar)
    new RotationMatrix(m).toQuaternion
  }
  
  val sidereal = Radians(-(ut1.toGMST.doubleValue.toRadians + dPsi * cos(ebar) + toRadians((0.00264 * sin(omega) + 0.000063 * sin(2 * omega)) / 3600.0))).rz
    
  val polar = new RotationMatrix(ImmutableArray(1,0,toRadians(xp / 3600.0),
                                                0,1,toRadians(-yp / 3600.0),
                                                toRadians(-xp / 3600.0),toRadians(yp / 3600.0),1)).toQuaternion
    
  /*
   * J2000/ECI : ECI = !precession*!nutation*!sidereal*!polar*ECEF
   * Mean of Date/MOD : precession*ECI = !nutation*!sidereal*!polar*ECEF
   * True of Date/TOD : nutation*precession*ECI = = nutation*MOD = !sidereal*!polar*ECEF
   * Pseudo Earth Fixed/PEF: sidereal*nutation*precession*ECI = sidereal*TOD = !polar*ECEF
   * True Earth Fixed/ITRF/ECEF: polar*sidereal*nutation*precession*ECI = polar*PEF = ECEF
   */
  
  //Conversions
  def J2000toITRF = polar*J2000toPEF
  def J2000toITRF(p : Vector3) : Vector3 = J2000toITRF * p
  val J2000toTOD = nutation*precession
  val TODtoJ2000 = !J2000toTOD
  def J2000toTOD(p : Vector3) : Vector3 = J2000toTOD * p
  def TODtoJ2000(p : Vector3) : Vector3 = TODtoJ2000 * p
  def J2000toPEF = sidereal*J2000toTOD 
  def J2000toPEF(p : Vector3) : Vector3 = J2000toPEF*p
  def PEFtoITRF(p : Vector3) = polar*p
  def PEFtoJ2000 = !J2000toPEF
  def PEFtoJ2000(p : Vector3) : Vector3 = PEFtoJ2000*p
  def TEMEtoPEF(p : Vector3) = Degrees(-ut1.toGMST.doubleValue).rz * p
  def TEMEtoJ2000(p : Vector3) = PEFtoJ2000(TEMEtoPEF(p))
  def ITRFtoJ2000 = !J2000toITRF
  def ITRFtoJ2000(p : Vector3) : Vector3 = ITRFtoJ2000*p
  def ITRFtoPEF(p : Vector3) = !polar*p
    
  //Velocity
  def J2000toITRF(p : Vector3, v : Vector3) : Array[Vector3] =
  {
    val pPEF = J2000toPEF(p)
    val vPEF = J2000toPEF(v)
    val vITRF = PEFtoITRF(vPEF - w % pPEF)
    val pITRF = PEFtoITRF(pPEF)
    Array[Vector3](pITRF, vITRF)
  }
  
  def J2000toITRF(eci : ECIStateVector) : ECEFStateVector =
  {
    val pPEF = J2000toPEF(eci.position)
    val vPEF = J2000toPEF(eci.velocity)
    val vITRF = PEFtoITRF(vPEF - w % pPEF)
    val pITRF = PEFtoITRF(pPEF)
    new ECEFStateVector(pITRF, vITRF)
  }

  def J2000toTOD(eci : ECIStateVector) : TODStateVector = new TODStateVector(J2000toTOD(eci.position), J2000toTOD(eci.velocity))
  def TODtoJ2000(tod : TODStateVector) : ECIStateVector = new ECIStateVector(TODtoJ2000(tod.position), TODtoJ2000(tod.velocity))

  def ITRFtoJ2000(p : Vector3, v : Vector3) : Array[Vector3] =
  {
    val pPEF = ITRFtoPEF(p)
    val vPEF = ITRFtoPEF(v) + w % pPEF
    val vJ2000 = PEFtoJ2000(vPEF)
    val pJ2000 = PEFtoJ2000(pPEF)
    Array[Vector3](pJ2000, vJ2000)
  }
  
  def ITRFtoJ2000(ecef : ECEFStateVector) : ECIStateVector =
  {
    val pPEF = ITRFtoPEF(ecef.position)
    val vPEF = ITRFtoPEF(ecef.velocity) + w % pPEF
    val vJ2000 = PEFtoJ2000(vPEF)
    val pJ2000 = PEFtoJ2000(pPEF)
    ECIStateVector(pJ2000, vJ2000)
  }
  
  def TEMEtoJ2000(p : Vector3, v : Vector3) : Array[Vector3] = Array[Vector3](TEMEtoJ2000(p), TEMEtoJ2000(v))
  def TEMEtoJ2000(teme : TEMEStateVector) : ECIStateVector = ECIStateVector( TEMEtoJ2000(teme.position), TEMEtoJ2000(teme.velocity))
}

object FK5 {
  //Vallado, Equations 3-57
  def zeta(t : Double) = ((0.6406161 + (0.0000839 + 5E-6 * t) * t) * t).toRadians
  def theta(t : Double) = ((0.5567530 - (0.0001185 - 1.16E-5 * t) * t) * t).toRadians
  def z(t : Double) = ((0.6406161 + (0.0003041 + 5.1E-6 * t) * t) * t).toRadians

  def ebar(t : Double) = toRadians((84381.448 - t * (46.8150 + t * (0.00059 - 0.001813 * t))) / 3600.0)
  def l(t : Double) = ((134.96340251 + t * (1717915923.2178 + t * (31.8792 + t * (0.051635 - 0.00024470 * t))) / 3600.0) % 360).toRadians
  def l1(t : Double) = ((357.52910918 + t * (129596581.0481 - t * (0.5532 + t * (0.000136 + 0.00001149 * t))) / 3600.0) % 360).toRadians
  def f(t : Double) = ((93.27209062 + t * (1739527262.8478 + t * (-12.7512 + t * (0.001037 + 0.00000417 * t))) / 3600.0) % 360).toRadians
  def d(t : Double) = ((297.85019547 + t * (1602961601.2090 + t * (-6.3706 + t * (0.006593 - 0.00003169 * t))) / 3600.0) % 360).toRadians
  def omega(t : Double) = ((125.04455501 + t * (-6962890.5431 + t * (7.4722 + t * (0.007702 - 0.00005939 * t))) / 3600.0) % 360).toRadians
  
  def dPsi_dEpsilon(t : Double, l : Double, l1 : Double, f : Double, d : Double, omega : Double) = ((0.0, 0.0) /: nut80){ (dpe, term) =>
    //Units here are radians
    val a = l * term(0) + l1 * term(1) + f * term(2) + d * term(3) + omega * term(4)
    //Units here are 1E-4 of an arcsecond.
    (dpe._1 + (term(5) + term(6) * t) * sin(a), dpe._2 + (term(7) + term(8) * t) * cos(a))
  } match { 
    //Convert to degrees then radians
    case (a, b) => (((a / 36000000.0) % 360).toRadians, ((b / 36000000.0) % 360).toRadians)
  }
 
  lazy val eopType = SGConf.getProperty("eop.type", "none")
  
  def applySTKEOP(time : JD) = {
    val eop = STKEarthOrientationParameters(time)
    new FK5(time, eop.dUT1, eop.DAT, eop.x, eop.y, eop.LOD)
  }

  def applyCelestrakEOP(time : JD) = {
    val eop = CelestrakEarthOrientationParameters(time)
    new FK5(time, eop.dUT1, eop.DAT, eop.x, eop.y, eop.LOD)
  }

//  def apply(time : JulianDate) = eopType match {
//    case "stk" => applySTKEOP(time)
//    case "celestrak" => applySTKEOP(time)
//    case "none" => new FK5(time, EmptyEOPEntry.dUT1, EmptyEOPEntry.DAT, EmptyEOPEntry.x, EmptyEOPEntry.y, EmptyEOPEntry.LOD)
//    case _ => new FK5(time, EmptyEOPEntry.dUT1, EmptyEOPEntry.DAT, EmptyEOPEntry.x, EmptyEOPEntry.y, EmptyEOPEntry.LOD)
//  }

  private val nut80 : Array[Array[Double]] = Array(
    // a1,a2,a3,a4,a5,Ai,Bi,Ci,Di
    Array(0, 0, 0, 0, 1, -171996, -174.2, 92025, 8.9),
    Array(0, 0, 0, 0, 2, 2062, 0.2, -895, 0.5),
    Array(-2, 0, 2, 0, 1, 46, 0, -24, 0),
    Array(2, 0, -2, 0, 0, 11, 0, 0, 0),
    Array(-2, 0, 2, 0, 2, -3, 0, 1, 0),
    Array(1, -1, 0, -1, 0, -3, 0, 0, 0),
    Array(0, -2, 2, -2, 1, -2, 0, 1, 0),
    Array(2, 0, -2, 0, 1, 1, 0, 0, 0),
    Array(0, 0, 2, -2, 2, -13187, -1.6, 5736, -3.1),
    Array(0, 1, 0, 0, 0, 1426, -3.4, 54, -0.1),
    Array(0, 1, 2, -2, 2, -517, 1.2, 224, -0.6),
    Array(0, -1, 2, -2, 2, 217, -0.5, -95, 0.3),
    Array(0, 0, 2, -2, 1, 129, 0.1, -70, 0),
    Array(2, 0, 0, -2, 0, 48, 0, 1, 0),
    Array(0, 0, 2, -2, 0, -22, 0, 0, 0),
    Array(0, 2, 0, 0, 0, 17, -0.1, 0, 0),
    Array(0, 1, 0, 0, 1, -15, 0, 9, 0),
    Array(0, 2, 2, -2, 2, -16, 0.1, 7, 0),
    Array(0, -1, 0, 0, 1, -12, 0, 6, 0),
    Array(-2, 0, 0, 2, 1, -6, 0, 3, 0),
    Array(0, -1, 2, -2, 1, -5, 0, 3, 0),
    Array(2, 0, 0, -2, 1, 4, 0, -2, 0),
    Array(0, 1, 2, -2, 1, 4, 0, -2, 0),
    Array(1, 0, 0, -1, 0, -4, 0, 0, 0),
    Array(2, 1, 0, -2, 0, 1, 0, 0, 0),
    Array(0, 0, -2, 2, 1, 1, 0, 0, 0),
    Array(0, 1, -2, 2, 0, -1, 0, 0, 0),
    Array(0, 1, 0, 0, 2, 1, 0, 0, 0),
    Array(-1, 0, 0, 1, 1, 1, 0, 0, 0),
    Array(0, 1, 2, -2, 0, -1, 0, 0, 0),
    Array(0, 0, 2, 0, 2, -2274, -0.2, 977, -0.5),
    Array(1, 0, 0, 0, 0, 712, 0.1, -7, 0),
    Array(0, 0, 2, 0, 1, -386, -0.4, 200, 0),
    Array(1, 0, 2, 0, 2, -301, 0, 129, -0.1),
    Array(1, 0, 0, -2, 0, -158, 0, -1, 0),
    Array(-1, 0, 2, 0, 2, 123, 0, -53, 0),
    Array(0, 0, 0, 2, 0, 63, 0, -2, 0),
    Array(1, 0, 0, 0, 1, 63, 0.1, -33, 0),
    Array(-1, 0, 0, 0, 1, -58, -0.1, 32, 0),
    Array(-1, 0, 2, 2, 2, -59, 0, 26, 0),
    Array(1, 0, 2, 0, 1, -51, 0, 27, 0),
    Array(0, 0, 2, 2, 2, -38, 0, 16, 0),
    Array(2, 0, 0, 0, 0, 29, 0, -1, 0),
    Array(1, 0, 2, -2, 2, 29, 0, -12, 0),
    Array(2, 0, 2, 0, 2, -31, 0, 13, 0),
    Array(0, 0, 2, 0, 0, 26, 0, -1, 0),
    Array(-1, 0, 2, 0, 1, 21, 0, -10, 0),
    Array(-1, 0, 0, 2, 1, 16, 0, -8, 0),
    Array(1, 0, 0, -2, 1, -13, 0, 7, 0),
    Array(-1, 0, 2, 2, 1, -10, 0, 5, 0),
    Array(1, 1, 0, -2, 0, -7, 0, 0, 0),
    Array(0, 1, 2, 0, 2, 7, 0, -3, 0),
    Array(0, -1, 2, 0, 2, -7, 0, 3, 0),
    Array(1, 0, 2, 2, 2, -8, 0, 3, 0),
    Array(1, 0, 0, 2, 0, 6, 0, 0, 0),
    Array(2, 0, 2, -2, 2, 6, 0, -3, 0),
    Array(0, 0, 0, 2, 1, -6, 0, 3, 0),
    Array(0, 0, 2, 2, 1, -7, 0, 3, 0),
    Array(1, 0, 2, -2, 1, 6, 0, -3, 0),
    Array(0, 0, 0, -2, 1, -5, 0, 3, 0),
    Array(1, -1, 0, 0, 0, 5, 0, 0, 0),
    Array(2, 0, 2, 0, 1, -5, 0, 3, 0),
    Array(0, 1, 0, -2, 0, -4, 0, 0, 0),
    Array(1, 0, -2, 0, 0, 4, 0, 0, 0),
    Array(0, 0, 0, 1, 0, -4, 0, 0, 0),
    Array(1, 1, 0, 0, 0, -3, 0, 0, 0),
    Array(1, 0, 2, 0, 0, 3, 0, 0, 0),
    Array(1, -1, 2, 0, 2, -3, 0, 1, 0),
    Array(-1, -1, 2, 2, 2, -3, 0, 1, 0),
    Array(-2, 0, 0, 0, 1, -2, 0, 1, 0),
    Array(3, 0, 2, 0, 2, -3, 0, 1, 0),
    Array(0, -1, 2, 2, 2, -3, 0, 1, 0),
    Array(1, 1, 2, 0, 2, 2, 0, -1, 0),
    Array(-1, 0, 2, -2, 1, -2, 0, 1, 0),
    Array(2, 0, 0, 0, 1, 2, 0, -1, 0),
    Array(1, 0, 0, 0, 2, -2, 0, 1, 0),
    Array(3, 0, 0, 0, 0, 2, 0, 0, 0),
    Array(0, 0, 2, 1, 2, 2, 0, -1, 0),
    Array(-1, 0, 0, 0, 2, 1, 0, -1, 0),
    Array(1, 0, 0, -4, 0, -1, 0, 0, 0),
    Array(-2, 0, 2, 2, 2, 1, 0, -1, 0),
    Array(-1, 0, 2, 4, 2, -2, 0, 1, 0),
    Array(2, 0, 0, -4, 0, -1, 0, 0, 0),
    Array(1, 1, 2, -2, 2, 1, 0, -1, 0),
    Array(1, 0, 2, 2, 1, -1, 0, 1, 0),
    Array(-2, 0, 2, 4, 2, -1, 0, 1, 0),
    Array(-1, 0, 4, 0, 2, 1, 0, 0, 0),
    Array(1, -1, 0, -2, 0, 1, 0, 0, 0),
    Array(2, 0, 2, -2, 1, 1, 0, -1, 0),
    Array(2, 0, 2, 2, 2, -1, 0, 0, 0),
    Array(1, 0, 0, 2, 1, -1, 0, 0, 0),
    Array(0, 0, 4, -2, 2, 1, 0, 0, 0),
    Array(3, 0, 2, -2, 2, 1, 0, 0, 0),
    Array(1, 0, 2, -2, 0, -1, 0, 0, 0),
    Array(0, 1, 2, 0, 1, 1, 0, 0, 0),
    Array(-1, -1, 0, 2, 1, 1, 0, 0, 0),
    Array(0, 0, -2, 0, 1, -1, 0, 0, 0),
    Array(0, 0, 2, -1, 2, -1, 0, 0, 0),
    Array(0, 1, 0, 2, 0, -1, 0, 0, 0),
    Array(1, 0, -2, -2, 0, -1, 0, 0, 0),
    Array(0, -1, 2, 0, 1, -1, 0, 0, 0),
    Array(1, 1, 0, -2, 1, -1, 0, 0, 0),
    Array(1, 0, -2, 2, 0, -1, 0, 0, 0),
    Array(2, 0, 0, 2, 0, 1, 0, 0, 0),
    Array(0, 0, 2, 4, 2, -1, 0, 0, 0),
    Array(0, 1, 0, 1, 0, 1, 0, 0, 0))
}