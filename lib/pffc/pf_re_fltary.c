/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_re_fltarray.c
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
#include "ds_structs.h"
#include "workspace.h"
#include   "re_defs.h"
#include    "u_defs.h"

#include <float.h>
#include <math.h>
#include <stdlib.h>

/*  Declare function */

#ifdef __STDC__

float *pf_re_fltarray ( PFFfid *fid, int keep, long *len, int *foff10, 
                         int *ierr );

float *pf_re_fltarray ( PFFfid *fid, int keep, long *len, int *foff10, 
                         int *ierr )

#else

float     *pf_re_fltarray  ();

float *pf_re_fltarray ( fid, keep, len, foff10, ierr )

PFFfid   *fid; 
int       keep;
long     *len;
int      *foff10;
int      *ierr;

#endif

/* Read a float array primitive (<FARRAY>) from a PFF file: 

    In FP_FULL precision, values are read from file as 32-bit IEEE floats.
    In FP_REDU precision (default), values are normalized to 16-bit 
    integers.  They are decoded via:

          FARRAY(i) = SCALE*int_array(i) + OFFSET

------------------------------------------------------------------------

    <FARRAY> Primitive Format:
    --------------------------
      IF ( Float_Precision = FULL )
        <INT>          FP_FULL
        <INT>          foff10
        <LONG>         LEN            array length
        <F4ARRAY>      array
      ELSE
        <FLOAT>        OFFSET         decoding offset value
        <FLOAT>        SCALE          decoding scale factor
        <LONG>         LEN            array length
        LOOP i=1,LEN
          <INT>        int_array(i)   ith integer value
        ENDLOOP
      ENDIF

------------------------------------------------------------------------

    Input:
      fid     -  pointer to PFF file structure
      keep    -  logical flag indicating wheter or not to keep a non-zero
                 value in the case of underflow
      ierr    -  If not zero, return with no operation

    Output:
      len     -  length of array read from PFF file
      foff10  -  power-of-ten multiplier of "farray"
      ierr    -  error flag:
                   =  0,  No Error
                   =  1,  Error allocating array

    Return Value:
      pointer to float array read from PFF file
*/
{
  static double   log2      = 0.30102999566398119521;
  static double   log215    = 4.51544993495971792821;
  static char    *module    = "PF_RE_FLTARRAY";
  register int    i;
  int             off10_off, off10_sca, off10;
  int             fp_mode;
  long            flen;
  float           offset, scale;
  double          logoff, logsca, logrng, biglog, roff10, logmax;
  float          *arrptr = NULL;
  int            *iptr = NULL;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return NULL;

  /* Read in precision flag (if present) and base-10 offset */
  pf_u_sio( fid, RE, 2, PFF_work_buf, ierr );

  if ( PFF_work_buf[0] < 0 )  {
    fp_mode = PFF_work_buf[0];
    *foff10 = PFF_work_buf[1];
  }
  else    {
    fp_mode = FP_REDU;

    /* Read in and decode "offset" and "scale" */
    pf_u_sio( fid, RE, 4, PFF_work_buf+2, ierr );
    pf_u_i2f( keep, PFF_work_buf, &offset, &off10_off, ierr);
    pf_u_i2f( keep, PFF_work_buf+3, &scale,  &off10_sca, ierr);
    
    /* Go thru machinations to produce a single power-of-ten offset for data */
    
    if ( offset == 0.0 )
      logoff = -1.e8;
    else
      logoff = log10( (double) ABS(offset) ) + off10_off;
    
    if ( scale == 0.0 )
      /* if scale = 0.0, only need to worry about "offset" */
      *foff10 = off10_off;
    else        {
      logsca = log10( (double) ABS(scale) ) + off10_sca;
      logrng = log215 + logsca;
      
      /* Find out if "offset" or "range" drives data upper bound; find upper 
      bound; select consistent power-of-ten offset   */
      if ( logoff > logrng )  {
        biglog = log2 + logoff;
        off10 = off10_off + 1;
      }
      else    {
        biglog = log2 + logrng;
        off10 = off10_sca + 5;
      }
      
      /* If possible overflow (or underflow w/ keep), set power-of-ten offset 
      to that calculated above; otherwise, set to zero  */
      if ( (biglog < log2*(FLT_MAX_EXP-1)) && 
        (  (keep == FALSE) || (biglog > log2*FLT_MIN_EXP) )  )
        off10 = 0;
      
      /* Adjust off10 if "scale" is in an underflow state  */
      if ( off10_sca < 0 )
        off10 = MIN ( off10_sca, off10);
      
      /* Adjust off10 if "offset" or "scale" will overflow */
      logmax = MAX( logoff, logsca );
      if ( (logmax - off10) >= log2*(FLT_MAX_EXP-1) )   {
        roff10 = logmax - log2*(FLT_MAX_EXP-1);
        if ( roff10 >= 0 )
          off10 = (int) (roff10 + 1);
        else
          off10 = (int) (roff10 + 0.0001);
      }
      
      /* If necessary, adjust "offset" and "scale" to have the proper 
      power-of-ten offset */
      if ( ( off10 != off10_off ) && ( offset != 0.0 ) )
        offset = (float) pow( 10.0, (double) (logoff - off10) );
      if ( ( off10 != off10_sca ) && ( scale != 0.0 ) )
        scale = (float) pow( 10.0, (double) (logsca - off10) );
      
      *foff10 = off10;
    }
  }

  if( *ierr != 0 ) return NULL;

  /* Read in and decode array length "len"  */
  pf_u_sio( fid, RE, 3, PFF_work_buf, ierr );
  pf_u_i2l( PFF_work_buf, len, ierr);

  if( *ierr == 0  && *len > 0 )   {
    flen = *len;
    if ( fp_mode == FP_FULL ) flen *= 2;

    if ( (arrptr = (float *) malloc((*len)*sizeof(float)) ) == NULL || 
         (iptr   = (int *)   malloc( (unsigned) flen*sizeof(int)) ) 
         == NULL  )  {
      *ierr = 1;
      free(iptr);
      free(arrptr);
      pf_wr_err ( module, *ierr, fid, "Error allocating array");
    }
    else {
      /* Now read in integer array */
      pf_u_sio( fid, RE, flen, iptr, ierr );

      if ( fp_mode == FP_REDU ) { /* FP_REDU floating point precision mode */
        /* Transfer ints to float array; then free integer array */
        for ( i=0; i<(*len); ++i)
          arrptr[i] = (scale*iptr[i]) + offset;
      }
      else   {      /* FP_FULL floating point precision mode */
        /* convert integer array to 4-byte float array */
        pf_u_f4 ( RE, *len, iptr, arrptr, ierr);
      }
    }
  }

  CHKFREE ( iptr );
  if ( *ierr ) {
    CHKFREE (arrptr);
    return NULL;
  }
  return arrptr;
}
