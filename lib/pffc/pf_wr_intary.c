/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_wr_intarray.c
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
#include "workspace.h"

#include    "u_defs.h"

/*  Declare function */

#ifdef __STDC__

void pf_wr_intarray ( PFFfid *fid, long len, int *iarr, int *ierr );

void pf_wr_intarray ( PFFfid *fid, long len, int *iarr, int *ierr )

#else

void       pf_wr_intarray  ();

void pf_wr_intarray ( fid, len, iarr, ierr )

PFFfid   *fid;
long      len;
int      *iarr;
int      *ierr;

#endif

/* Write an integer array primitive (<IARRAY>) to a PFF file: 

------------------------------------------------------------------------

    <IARRAY> Primitive Format:
    --------------------------
     IF (integer_precision = 4-byte)
       <INT>          INTP_4
       <LONG>         LEN          array length
       LOOP i=1,LEN       \
         <INT>,<INT>   IARR[i]     ith integer value (4-byte)
        ENDLOOP         /
      ELSE
        <LONG>         LEN          array length
        LOOP i=1,LEN
          <INT>        IARR[i]     ith integer value (2-byte)
        ENDLOOP
      ENDIF

------------------------------------------------------------------------

    Input:
      fid     -  pointer to PFF file structure
      len     -  length of array to be written to PFF file
      iarr    -  pointer to integer array to be written to PFF file
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   =  0,  No Error
                   =  otherwise,  Error from called PFF routine
*/
{
  int ib, ilast, flen;
  int MAXSI2 = 32767, MINSI2 = -32768;
  int use_i4 = 0;

  /* static char    *module    = "PF_WR_INTARRAY"; */

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  for(ib=0; ib<len; ++ib) {
    if (iarr[ib] > MAXSI2 || iarr[ib] < MINSI2 ) {
      use_i4 = 1;
      break;
    }
  }

  if ( use_i4 )   {
    /* write out full-precision flag offset */
    PFF_work_buf[0] = INTP_4;
    pf_u_sio( fid, WR, 1, PFF_work_buf, ierr );
    flen = 2*len;
  }
  else   {
    flen = len;
  }

  /* Encode and write out the array length "len"  */
  pf_u_l2i( len, PFF_work_buf, ierr);
  pf_u_sio( fid, WR, 3, PFF_work_buf, ierr );

  /* If no error, write out the integer array */
  if( *ierr == 0 ) {
    if ( use_i4 ) {
      for(ib=0; ib<len; ib+=PFF_RECLEN/2) {
        ilast = MIN(len,ib+PFF_RECLEN/2);
        pf_u_i4 ( WR, ilast-ib, PFF_work_buf, iarr+ib, ierr );
        pf_u_sio( fid, WR, 2*(ilast-ib), PFF_work_buf, ierr );
      }
    }
    else  pf_u_sio( fid, WR, len, iarr, ierr );
  }
  return;
}
