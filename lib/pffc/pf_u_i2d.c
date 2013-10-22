/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_u_i2d.c
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

void pf_u_i2d ( int *ival, double *dval, int *ierr );

void pf_u_i2d ( int *ival, double *dval, int *ierr )

#else

void       pf_u_i2d       ();

void pf_u_i2d ( ival, dval, ierr )

int   *ival;
float *dval;
int   *ierr;

#endif

/* 
       - This routine is a UTILITY routine to decode a double floating
         point value from five 15-bit unsigned integers
            ival[0] = 16 most significant bits of mantissa (with most 
                      significant bit hidden/zeroed)
            ival[1] = 15 next most significant bits of mantissa
            ival[2] = 15 next most significant bits of mantissa
            ival[3] = remaining bits of mantissa
            ival[4] = low bit    --  sign bit
                      bits 2-15  --  excess-8192 base-2 exponent
       - If the excess-8192 exponent is zero, the float value is 0.0.

    Input:
      ival    -  input integer array (length 5)
      ierr    -  If not zero, return with no operation

    Output:
      dval    -  output float
      ierr    -  error flag:
                   = 0,  Normal return
                   = 1,  Encoded Integers Signed or Larger Than 15-bits
*/
{
  static char    *module    = "PF_U_I2F";
  static int     two15    = 32768;
  static double   rtwo15    = 3.0517578125000000e-5;

  int             ie, is, tmin, tmax;
  double          xsign, y;

  tmin = ival[0];
  tmax = ival[0];
  for (ie = 1; ie < 5; ++ie){
    tmin = MIN(tmin,ival[ie]); 
    tmax = MAX(tmax,ival[ie]); 
  }
  if ( tmin < 0 || tmax >= two15) {
    *ierr = 1;
    pf_wr_err ( module, *ierr, NULL, 
                      "Encoded Integers Signed or Larger Than 15-bits" );
    return;
  }
  
  /* extract excess-8192 exponent, dval=0 if this is zero */

  ie = ival[4] >> 1;

  if ( ie == 0 )  {
    *dval = 0.0;
  }

  /* Otherwise, reconstruct # from sign, power-of-two, and mantissa */

  else {
    ie -= 8193; 
    is  =  ival[4] & 1;
    xsign = 1 - 2*is;
    xsign = xsign*pow( 2.0, (double) ie );

    *dval = (double) (xsign*
               ((((ival[3]*rtwo15  + ival[2])*rtwo15 +
                   ival[1])*rtwo15 + ival[0])*rtwo15 + 1.0));
  }

  return;
}
