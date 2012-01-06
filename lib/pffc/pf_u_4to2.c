/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_u_4to2.c
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

void pf_u_4to2 ( int iop, long len, int *ibuf, short int *shrt);

void pf_u_4to2 ( int iop, long len, int *ibuf, short int *shrt)

#else

void       pf_u_4to2       ();

void pf_u_4to2 ( iop, len, ibuf, shrt )

int          iop, *ibuf;
short int   *shrt;
long     len;

#endif

/* Helper routine that moves integers between an array of 4-byte integers
   and an array of 2-byte integers, with a potential pair-wise word flip
   if byte-swapping is needed. The calling program casts a 4-byte integer
   or 4-byte float array to "shrt", so that each 4-byte value is split
   between two words of the "ibuf" array. This gives a simple way of
   passing IEEE floats and ones-complement integers using PFF's underlying
   2-byte integer datastream.

    Input:
      iop     -  opcode  ( RE or WR )
      len     -  number of integer pairs to be converted
      ibuf    -  pointer to int buffer (iop = RE only)
      shrt    -  pointer to short int array (iop = WR only)

    Output:
      ibuf    -  pointer to int buffer (iop = WR only)
      shrt    -  pointer to short int array (iop = RE only)
*/
{
  register int        j;
  int                 flip;

# ifdef HU_ENDIAN_IS_LSB_FIRST
  flip = 1;
# else
  flip = 0;
# endif

  if ( iop == RE )  {

    for (j=0; j<2*len; ++j)   {
      shrt[j] = ibuf[j+flip];
      ++j;
      shrt[j] = ibuf[j-flip];
    }
  }
  else if ( iop == WR )  {

    for (j=0; j<2*len; ++j)   {
      ibuf[j+flip] = shrt[j];
      ++j;
      ibuf[j-flip] = shrt[j];
    }
  }
  return;
}
