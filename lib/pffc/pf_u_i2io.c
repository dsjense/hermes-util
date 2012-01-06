/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_u_i2io.c
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

#ifdef __PGI
# define EXTEND_SIGN(A) if ((A) & 0x8000) A |= 0xffff0000
#else
# define EXTEND_SIGN(A)
#endif

#include "pff.h"

/*  Declare function */

#ifdef __STDC__

void pf_u_i2io ( FILE *file, PFFfid *fid, int iop, long len, int *iarr, 
                 int *ierr);

void pf_u_i2io ( FILE *file, PFFfid *fid, int iop, long len, int *iarr, 
                 int *ierr)

#else

void       pf_u_i2io       ();

void pf_u_i2io ( file, fid, iop, len, iarr, ierr )

FILE    *file;
PFFfid  *fid;
int      iop, *iarr, *ierr;
long     len;

#endif

/* converts array of floats to/from an array of ints (2 ints per float)

    Input:
      file    -  pointer to FILE (as returned by fopen)
      fid     -  input file ID
      iop     -  opcode  ( RE or WR )
      len     -  number of integers to be read/written
      iarr    -  pointer to int array (iop = WR only)
      ierr    -  If not zero, return with no operation

    Output:
      iarr    -  pointer to int array (iop = RE only)
      ierr    -  error flag:
                   = -1,  EOF encountered while reading from file
                   =  0,  Normal return
                   =  1,  I/O Error on Read
                   =  2,  I/O Error on Write
                   =  3,  Illegal Opcode
*/
{
  register int        i;
  short int          *buf;
  static char        *module    = "PF_U_I2IO";
  size_t              ferr;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  if ( iop == RE || iop == WR )  {

    /* Set internal buffer to first part of target buffer */
    buf = (short int *) iarr;

    if ( iop == RE )  {

      /* Load binary data */
      ferr = fread( (char *)buf, 2, len, file );
      /* Check for error */
      if ( ferr < len )   { 
        if ( feof(file) != 0 ) {
          *ierr = -1;
          return;
        }
        else if ( ferror(file) != 0 ) {
          *ierr = 1;
          pf_wr_err ( module, *ierr, fid, 
                                 "Error encountered while reading from file" );
          return;
        }
      }
    }
    else if ( iop == WR )  {
 
      /* Pack source array into internal buffer values */
      for (i=0; i<len; ++i)
#ifdef HU_ENDIAN_IS_LSB_FIRST
        buf[i] = ((255&iarr[i])<<8) | (255&(iarr[i]>>8));   
#else
        buf[i] = iarr[i];
#endif

      /* Load binary data */
      ferr = fwrite( (char *)buf, 2, len, file );
      /* Check for error */
      if ( ferr < len )    {
        *ierr = 2;
        pf_wr_err ( module, *ierr, fid, 
                                "Error encountered while writing to file" );
        return;
      }
    }
    /* Expand internal buffer values to target buffer */
    for (i=len-1; i>=0; --i)
#ifdef HU_ENDIAN_IS_LSB_FIRST
    {
      iarr[i] = (short) ((255&buf[i])<<8) | (255&(buf[i]>>8));
      EXTEND_SIGN(iarr[i]);
    }
#else
                       iarr[i] = buf[i];
#endif
  }

  else    {
    
    *ierr=3;
    pf_wr_err ( module, *ierr, NULL, "Illegal Opcode" );
  }
  return;
}
