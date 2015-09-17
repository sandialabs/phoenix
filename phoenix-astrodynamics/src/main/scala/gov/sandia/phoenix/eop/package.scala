package gov.sandia.phoenix

/**
 * Classes for correct handling of Earth Orientation Parameters (EOPs). These are necessary for handling of polar motion
 * calculations when doing the FK5 reduction. A few things to remember:
 * $ EOPs are tabulated for historic values and estimated into the near future.
 * $ When comparing results, be sure to use the same EOP file.
 * $ When in doubt, use all 0s for polar motion to ensure consistency.
 */
package object eop