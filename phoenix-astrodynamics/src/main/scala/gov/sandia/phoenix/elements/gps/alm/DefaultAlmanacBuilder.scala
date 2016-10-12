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

package gov.sandia.phoenix.elements.gps.alm

class DefaultAlmanacBuilder {
        var _PRN : Int = _
        var SOH : Byte = 1
        var m0 : Double = _        // Mean anomaly at refernce time.
        var e : Double = _         // Eccentricity.
        var sqrt_a : Double = _    // Square root of semi-major axis.
        var omega_0 : Double = _   // Longitude of ascending node of orbit plane at weekly epoch.
        var i0 : Double = _        // Inclination angle at reference time.
        var w : Double = _         // Argument of perigee.
        var omega_dot : Double = _ // Rate of right ascension.
        var t_oa : Float = _        // Clock data reference time.
        var _af1 : Float = _        // Polynomial coefficient (SV clock correction)
        var _af0 : Float = _        // Polynomial coefficient (SV clock correction)
        var _weekNumber : Int = _

       def build : Almanac = new DefaultAlmanac(_PRN, SOH, e, t_oa, i0, omega_dot, sqrt_a, omega_0, w, m0, _af0, _af1, _weekNumber)

        def meanAnomaly(M0 : Double)=
        {
            this.m0 = M0
            this
        }

        def stateOfHealth(SOH : Byte)=
        {
            this.SOH = SOH
            this
        }

        def eccentricity(e : Double)=
        {
            this.e = e
            this
        }

        def timeOfApplicability(t_oa : Float)=
        {
            this.t_oa = t_oa
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

        def PRN(prn : Int)=
        {
            _PRN = prn
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

        def weekNumber(weekNumber : Int)=
        {
            _weekNumber = weekNumber
            this
        }
    }