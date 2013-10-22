/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_u_d2i.c
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

#include <math.h>
#include "pff.h"

/*  Declare function */

#ifdef __STDC__

void pf_u_d2i ( double dval, int *ival, int *ierr );

void pf_u_d2i ( double dval, int *ival, int *ierr )

#else

void       pf_u_d2i       ();

void pf_u_d2i ( dval, ival, ierr )

double dval;
int   *ival;
int   *ierr;

#endif

/* 
       - This routine is a UTILITY routine to encode a double floating
         point value into five 15-bit unsigned integers.
            ival[0] = 16 most significant bits of mantissa (with most 
                      significant bit hidden/zeroed)
            ival[1] = 15 next most significant bits of mantissa
            ival[2] = 15 next most significant bits of mantissa
            ival[3] = remaining bits of mantissa
            ival[4] = low bit    --  sign bit
                      bits 2-15  --  excess-8192 base-2 exponent
(
       - If the excess-8192 exponent is zero, the float value is 0.0.

    Input:
      dval    -  double value to be encoded
      ierr    -  If not zero, return with no operation

    Output:
      ival    -  output array of 5 encoded integers
      ierr    -  error flag:
                   = 0,  Normal return
                   = 1,  Float Value Out of Range
*/
{
  static char    *module    = "PF_U_D2I";
  static int      excess    = 8192;
  static double   two15     = 32768.0;
  static double   ln2       = 0.69314718055994530942;
  static double   rln2      = 1.44269504088896340736;

  register int    i;
  int             n;
  double          f15, frac, r, xd, y;

  if ( dval == 0.0 )
    for ( i=0;i<5;++i)
      ival[i] = 0;

  else    {

    xd = ABS ( dval );
    y  = rln2*log(xd);
    n  = (int) y;

    if ( n <= -excess )
      for ( i=0;i<5;++i)
        ival[i] = 0;

    else if ( n >= excess ) {
      *ierr = 1;
      pf_wr_err ( module, *ierr, NULL, "double Value Out of Range" );
    }
    else {
      r = y - n;

      if ( r < 0.0 )
        r += 1.0;
      else
        ++n;

      frac = MAX ( exp(ln2*r) - 1.0 , 0.0 );

      f15  = two15*frac;
      ival[0] = (int) f15;
      ival[1] = (int) (two15*(f15 - ival[0]));
      ival[2] = (int) two15*(two15*(f15 - ival[0])-ival[1]);
      ival[3] = (int) two15*(two15*(two15*(f15-ival[0])-ival[1])-ival[2]);
      ival[4] = 2*(n + excess) + ( ( dval < 0.0 ) ? 1 : 0 );
    }
  }

  return;
}
