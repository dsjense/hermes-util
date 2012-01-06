/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_u_l2i.c
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

void pf_u_l2i ( long lval, int *ival, int *ierr );

void pf_u_l2i ( long lval, int *ival, int *ierr )

#else

void       pf_u_l2i       ();

void pf_u_l2i ( lval, ival, ierr )

long  lval;
int  *ival;
int  *ierr;

#endif

/* convert a long integer to a PFF 3-integer value 

    Input:
      lval    -  input long integer
      ierr    -  If not zero, return with no operation

    Output:
      ival    -  output integer array (length 3)
      ierr    -  error flag:
                   = 0,  Normal return
                   = 1,  Long Integer out of range
*/
{
  static char    *module    = "PF_U_L2I";
  static int      imsk[3]   = { 16384, 32768, 32768 };
  register int    i;
  long            tval,tvaln;

  tval = ABS(lval);

  for (i=2;i>=0;i--)  {
    if ( tval>=imsk[i] ) {
      tvaln   = tval/imsk[i];
      ival[i] = tval - imsk[i]*tvaln;
      tval    = tvaln;
    }
    else   {
      ival[i] = tval;
      tval    = 0;
    }
  }
  if ( tval != 0 )   {
    *ierr = 1;
    pf_wr_err ( module, *ierr, NULL, "Long Integer out of range" );
    return;
  }

  if ( lval < 0 )
    ival[0] += imsk[0];
  
  return;
}
