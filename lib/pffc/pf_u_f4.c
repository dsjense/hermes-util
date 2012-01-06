/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_u_f4.c
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
#include "u_defs.h"

/*  Declare function */

#ifdef __STDC__

void pf_u_f4 ( int iop, long len, int *ibuf, float *farr, int *ierr);

void pf_u_f4 ( int iop, long len, int *ibuf, float *farr, int *ierr)

#else

void       pf_u_f4       ();

void pf_u_f4 ( iop, len, ibuf, farr, ierr )

int      iop, *ibuf, *ierr;
float   *farr;
long     len;

#endif

/* converts array of floats to/from an array of ints (2 ints per float)

    Input:
      iop     -  opcode  ( RE or WR )
      len     -  number of floats to be converted
      ibuf    -  pointer to int buffer (iop = RE only)
      farr    -  pointer to float array (iop = WR only)
      ierr    -  If not zero, return with no operation

    Output:
      ibuf    -  pointer to int buffer (iop = WR only)
      farr    -  pointer to float array (iop = RE only)
      ierr    -  error flag:
                   = 0,  Normal return
                   = 1,  Illegal Opcode
                   = 2,  Not implemented
*/
{
  static char        *module    = "PF_U_F4";
#ifdef F4_MODE_NOT_IMPLEMENTED

  *ierr=2;
  pf_wr_err ( module, *ierr, NULL, "F4 mode not implemented" );

#else
  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  if ( iop == RE || iop == WR )  {
    /* call converter routine */
    pf_u_4to2 ( iop, len, ibuf, (short int *) farr );
  }
  else    {
    *ierr=1;
    pf_wr_err ( module, *ierr, NULL, "Illegal Opcode" );
  }
#endif
  return;
}
