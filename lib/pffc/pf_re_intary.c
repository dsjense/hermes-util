/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_re_intarray.c
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

#include <stdlib.h>
#include "pff.h"
#include "workspace.h"

#include    "u_defs.h"

/*  Declare function */

#ifdef __STDC__

int *pf_re_intarray ( PFFfid *fid, long *len, int *ierr );

int *pf_re_intarray ( PFFfid *fid, long *len, int *ierr )

#else

int       *pf_re_intarray  ();

int *pf_re_intarray ( fid, len, ierr )

PFFfid   *fid; 
long     *len;
int      *ierr;

#endif

/* Read an integer array primitive (<IARRAY>) from a PFF file: 

    In INTP_2 precision (default), values are read from file as 2-byte ints.
    In INTP_4 precision, values are read from file as 4-byte ints.

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
      ierr    -  If not zero, return with no operation

    Output:
      len     -  length of array read from PFF file
      ierr    -  error flag:
                   =  0,  No Error
                   =  1,  Error allocating array

    Return Value:
      pointer to integer array read from PFF file
*/
{
  static char    *module    = "PF_RE_INTARRAY";
  int             ip_mode, flen;
  int            *arrptr = NULL;
  int            *iptr   = NULL;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return NULL;

  /* Read in precision flag (if present) */
  pf_u_sio( fid, RE, 1, PFF_work_buf, ierr );

  if ( PFF_work_buf[0] < 0 )  {
    ip_mode = PFF_work_buf[0];
    /* now read length */
    pf_u_sio( fid, RE, 3, PFF_work_buf, ierr );
  }
  else    {
    ip_mode = INTP_2;
    /* now read last two words of length */
    pf_u_sio( fid, RE, 2, PFF_work_buf+1, ierr );
  }

  /* decode array length "len"  */
  pf_u_i2l( PFF_work_buf, len, ierr);

  if( *ierr == 0  && *len > 0 )   {
    flen = *len;
    if ( ip_mode == INTP_4 ) flen *= 2;

    /* try to get storage for integer array */
    if ( (iptr = (int *) malloc( (unsigned) flen*sizeof(int) )) == NULL )  {
      *ierr = 1;
      pf_wr_err ( module, *ierr, fid, "Error allocating array");
    }
    else    {
      /* Now read in integer array */
      pf_u_sio( fid, RE, flen, iptr, ierr );
      if ( ip_mode == INTP_2 ) {
        arrptr = iptr;
      }
      else {
        if ( (arrptr = (int *) malloc( (unsigned) (*len)*sizeof(int)) ) 
             == NULL  )  {
          *ierr = 1;
          free(iptr);
          pf_wr_err ( module, *ierr, fid, "Error allocating array");
        }
        else {
          /* convert 2-byte int array to 4-byte int array */
          pf_u_i4 ( RE, *len, iptr, arrptr, ierr);
          free(iptr); iptr = NULL;
        }
      }
      if( *ierr != 0 )   {
        free( arrptr );
        free( iptr );
        arrptr = NULL;
      }
    }
  }
  return arrptr;
}
