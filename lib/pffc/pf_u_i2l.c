/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_u_i2l.c
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

/*  Declare function */

#ifdef __STDC__

void pf_u_i2l ( int *ival, long *lval, int *ierr );

void pf_u_i2l ( int *ival, long *lval, int *ierr )

#else

void       pf_u_i2l       ();

void pf_u_i2l ( ival, lval, ierr )

int  *ival;
long *lval;
int  *ierr;

#endif

/* convert a PFF 3-integer value to a long integer 

    Input:
      ival    -  input integer array (length 3)
      ierr    -  If not zero, return with no operation

    Output:
      lval    -  output long integer
      ierr    -  error flag:
                   = 0,  Normal return
                   = 1,  Encoded Integers Signed or Larger Than 15-bits
*/
{
  static char    *module    = "PF_U_I2L";
  static int      imsk[3]   = { 16384, 32768, 32768 };

  if ( (MIN(ival[0],MIN(ival[1],ival[2])) < 0) || 
       (MAX(ival[0],MAX(ival[1],ival[2])) >= imsk[2]) )   {
    *ierr = 1;
    pf_wr_err ( module, *ierr, NULL, 
                      "Encoded Integers Signed or Larger Than 15-bits" );
    return;
  }
  
  *lval = ( (ival[0] % imsk[0])*imsk[1] + ival[1] )*imsk[2] + ival[2];

  if ( ival[0] >= imsk[0] )
    *lval = -(*lval);

  return;
}
