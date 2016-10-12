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

package gov.sandia.phoenix

/**
 * Classes for correct handling of Earth Orientation Parameters (EOPs). These are necessary for handling of polar motion
 * calculations when doing the FK5 reduction. A few things to remember:
 * $ EOPs are tabulated for historic values and estimated into the near future.
 * $ When comparing results, be sure to use the same EOP file.
 * $ When in doubt, use all 0s for polar motion to ensure consistency.
 */
package object eop