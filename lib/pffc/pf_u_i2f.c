/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_u_i2f.c
-------------------------------------------------------------------------------
      $Id$
      
      Copyright (2008) Sandia Corporation. Under the terms of
      Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
      Government retains certain rights in this software.
      
      Hermes is free software: you can redistribute it and/or modify
      it under the terms of the GNU Lesser General Public License as
      published by the Free Software Foundation, either version 3 of
      the License, or (at your option) any later version.
      
      Hermes is distributed in the hope that it will be useful, but
      WITHOUT ANY WARRANTY; without even the implied warranty of
      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      GNU Lesser General Public License for more details.
      
      You should have received a copy of the GNU Lesser General
      Public License along with Hermes.  If not, see
      <http://www.gnu.org/licenses/>.
      
-------------------------------------------------------------------------------
*/

#include "pff.h"

#include <float.h>
#include <math.h>

/*  Declare function */

#ifdef __STDC__

void pf_u_i2f ( int keep, int *ival, float *xval, int *off10, int *ierr );

void pf_u_i2f ( int keep, int *ival, float *xval, int *off10, int *ierr )

#else

void       pf_u_i2f       ();

void pf_u_i2f ( keep, ival, xval, off10, ierr )

int    keep;
int   *ival;
float *xval;
int   *off10;
int   *ierr;

#endif

/* 
       - This routine is a UTILITY routine to decode a floating point 
         number from three 15-bit unsigned integers.  The decoded float 
         value is returned with XVAL and OFF10 and given by:
                       float value = XVAL * 10.**OFF10
       - The integer OFF10 is provided to allow machines with with 
         floating point ranges comparable to VAX (0.29e-38 - 1.7e38) to 
         decode numbers with higher (Cray-type) ranges.  Encoded 
         integers contain:
            ival[0] = 16 most significant bits of mantissa (with most 
                      significant bit hidden/zeroed)
            ival[1] = 15 next most significant bits of mantissa
            ival[2] = low bit    --  sign bit
                      bits 2-15  --  excess-8192 base-2 exponent
       - If the excess-8192 exponent is zero, the float value is 0.0.
       - If IVAL[2]/2 < PFF_EXP2_MIN, then
                If KEEP = TRUE,   OFF10 = IVAL[2]/2 and non-zero XVAL 
                                    is returned
                If KEEP = FALSE,  XVAL = 0.0 and OFF10 = 0 are 
                                    returned 
       - Two machine-dependent parameters, PFF_EXP2_MIN and PFF_EXP2_MAX, 
         the minimum and maximum base-2 exponents that are within the range 
         of the machine's default FLOAT data type, must be defined.

    Input:
      keep    -  logical flag indicating whether or not to keep precision in 
                 case of underflow
      ival    -  input integer array (length 3)
      ierr    -  If not zero, return with no operation

    Output:
      xval    -  output float
      off10   -  power-of-ten offset for xval, zero returned unless xval would 
                 otherwise be out of machine range
      ierr    -  error flag:
                   = 0,  Normal return
                   = 1,  Encoded Integers Signed or Larger Than 15-bits
*/
{
  static char    *module    = "PF_U_I2F";
  static int      two15     = 32768;
  static double   rtwo15    = 3.0517578125e-5;
  static double   log2      = 0.30102999566398119521;
  static double   rlog2     = 3.32192809488736234787;

  int             i2m, i2rest, i2x, ie, is;
  double          twooff, xsign, y;

  if ( (MIN(ival[0],MIN(ival[1],ival[2])) < 0) || 
       (MAX(ival[0],MAX(ival[1],ival[2])) >= two15) )   {
    *ierr = 1;
    pf_wr_err ( module, *ierr, NULL, 
                      "Encoded Integers Signed or Larger Than 15-bits" );
    return;
  }
  
  /* extract excess-8192 exponent and sign bit from 3rd integer */
  ie = ( ival[2] >> 1 ) - 8192;
  is =  ival[2] & 1;

  xsign = 1 - 2*is;
  i2m = ie - 1;

  if ( ie == -8192 )  {
    *xval = 0.0;
    *off10 = 0;
    return;
  }

  if ( ie > FLT_MAX_EXP-1 )  {
    *off10 = (int) (i2m*log2);
    y      =  (double) i2m - (*off10)*rlog2;
    twooff = pow( 2.0, y);
  }
  else if ( ie < FLT_MIN_EXP )  {
    if ( keep == TRUE )  {
      *off10 = (int) (-i2m*log2);
      y      =  (double) i2m + (*off10)*rlog2;
      twooff = pow( 2.0, y);
      *off10 = -(*off10);
    }
    else  {
      *xval = 0.0;
      *off10 = 0;
      return;
    }
  }
  else  {
    *off10 = 0;
    i2x = MAX( i2m, FLT_MIN_EXP + 2 );
    i2rest = i2m - i2x;
    xsign = xsign*pow( 2.0, (double) i2rest );
    twooff = pow( 2.0, (double) i2x);
  }
  
  /* reconstruct floating value; use double precision for mantissa to 
     preserve low-order bits on 32-bit machines  */

  *xval = (float) (xsign*
           ( ( (ival[1]*rtwo15 + ival[0])*rtwo15 + 1.0 ) * twooff ) );

  return;
}
