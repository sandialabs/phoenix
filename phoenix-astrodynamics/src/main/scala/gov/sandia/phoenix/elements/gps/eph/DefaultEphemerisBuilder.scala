/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package gov.sandia.phoenix.elements.gps.eph

class DefaultEphemerisBuilder
{
  var _SVN : Int = _
  var iodc : Int = _
  var iode : Byte = _
  var toe: Float = _        // Time of ephemeris.
  var m0: Double = _        // Mean anomaly at refernce time.
  var delta_n: Float = _    // Mean motion difference from computed value.
  var e: Double = _         // Eccentricity.
  var sqrt_a: Double = _    // Square root of semi-major axis.
  var omega_0: Double = _   // Longitude of ascending node of orbit plane at weekly epoch.
  var i0: Double = _        // Inclination angle at reference time.
  var w: Double = _         // Argument of perigee.
  var omega_dot: Double = _ // Rate of right ascension.
  var i_dot : Double = _     // Rate of inclination angle.
  var _cuc: Float = _        // Amplitude of the cosine harmonic correction term to the argument of latitude.
  var _cus: Float = _        // Amplitude of the sine harmonic correction term to the argument of latitude.
  var _crc: Float = _        // Amplitude of the cosine harmonic correction term to the orbit radius.
  var _crs: Float = _        // Amplitude of the sine harmonic correction term to the orbit radius.
  var _cic: Float = _        // Amplitude of the cosine harmonic correction term to the angle of inclination.
  var _cis: Float = _        // Amplitude of the sine harmonic correction term to the angle of inclination.
  var tgd: Float = _        // Estimated group delay differential.
  var toc: Float = _        // Clock data reference time.
  var _af2: Float = _        // Polynomial coefficient (SV clock correction)
  var _af1: Float = _        // Polynomial coefficient (SV clock correction)
  var _af0: Float = _        // Polynomial coefficient (SV clock correction)
  var weekNumber : Int = _   //Week number of ephemeris
  var l2code : Byte = _
  var svacc : Byte = _
  var l2pdata: Boolean = _
  var soh : Byte = _

  def build= new DefaultEphemeris(_SVN, iodc, iode, toe, m0, delta_n, e,
                                sqrt_a, omega_0, i0, w, omega_dot, i_dot, _cuc, _cus, _crc,
                                _crs, _cic, _cis, tgd, toc, _af2, _af1, _af0, weekNumber, l2code,
                                svacc, l2pdata, soh)

  def meanAnomaly(M0 : Double)=
  {
    this.m0 = M0
    this
  }

  def meanMotionDifference(delta_n : Float)=
  {
    this.delta_n = delta_n
    this
  }

  def eccentricity(e : Double)=
  {
    this.e = e
    this
  }

  def squareRootOfSemiMajorAxis(sqrt_a : Double)=
  {
    this.sqrt_a = sqrt_a
    this
  }

  def longitudeOfAscendingNode(omega_0 : Double)=
  {
    this.omega_0 = omega_0
    this
  }

  def inclination(i0 : Double)=
  {
    this.i0 = i0
    this
  }

  def argumentOfPerigee(w : Double)=
  {
    this.w = w
    this
  }

  def rateOfRightAscension(omega_dot : Double)=
  {
    this.omega_dot = omega_dot
    this
  }

  def rateOfInclination(i_dot : Double)=
  {
    this.i_dot = i_dot
    this
  }

  def cuc(cuc : Float)=
  {
    _cuc = cuc
    this
  }

  def cus(cus : Float)=
  {
    _cus = cus
    this
  }

  def crc(crc : Float)=
  {
    _crc = crc
    this
  }

  def crs(crs : Float)=
  {
    _crs = crs
    this
  }

  def cic(cic : Float)=
  {
    _cic = cic
    this
  }

  def cis(cis : Float)=
  {
    _cis = cis
    this
  }

  def timeOfEphemeris(toe : Float)=
  {
    this.toe = toe
    this
  }

  def issueOfData(iode : Byte)=
  {
    this.iode = iode
    this
  }

  def SVN(svn : Int)=
  {
    _SVN = svn
    this
  }

  def L2Code(l2code : Byte)=
  {
    this.l2code = l2code
    this
  }

  def WeekNumber(weekNumber : Int)=
  {
    this.weekNumber = weekNumber
    this
  }

  def L2PDataFlag(l2pdata : Boolean)=
  {
    this.l2pdata = l2pdata
    this
  }

  def SVAccuracy(svacc : Byte)=
  {
    this.svacc = svacc
    this
  }

  def SVHealth(soh : Byte)=
  {
    this.soh = soh
    this
  }

  def Tgd(tgd : Float)=
  {
    this.tgd = tgd
    this
  }

  def IssueOfDataClock(iodc : Int)=
  {
    this.iodc = iodc
    this
  }

  def TimeOfCollection(toc : Float)=
  {
    this.toc = toc
    this
  }

  def af0(af0 : Float)=
  {
    _af0 = af0
    this
  }

  def af1(af1 : Float)=
  {
    _af1 = af1
    this
  }

  def af2(af2 : Float)=
  {
    _af2 = af2
    this
  }
}