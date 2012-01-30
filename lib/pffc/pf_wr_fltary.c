/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_wr_fltarray.c
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
#include "pffmp.h"
#include "workspace.h"
#include    "u_defs.h"
#include   "wr_defs.h"

#include <assert.h>
#include <float.h>
#include <stdlib.h>

/*  Declare function */

#ifdef __STDC__

long pf_wr_fltarray ( PFFfid *fid, int precision, long len, float *farray,
                      int foff10, int *ierr );

long pf_wr_fltarray ( PFFfid *fid, int precision, long len, float *farray,
                      int foff10, int *ierr )

#else

long pf_wr_fltarray ();

long pf_wr_fltarray ( fid, precision, len, farray, foff10, ierr )

PFFfid   *fid; 
int        precision;
long      len;
float    *farray;
int       foff10;
int      *ierr;

#endif

/* 
   Write a float array primitive (<FARRAY>) to a PFF file: 

    Values are normalized to 16-bit integers.  They are decoded via:

          FARRAY(i) = SCALE*int_array(i) + OFFSET

------------------------------------------------------------------------

    <FARRAY> Primitive Format:
    --------------------------
      IF ( Float_Precision = FULL )
        <FLOAT>        OFFSET         decoding offset value
        <FLOAT>        SCALE          decoding scale factor
        <LONG>         LEN            array length
        LOOP i=1,LEN
          <INT>        int_array(i)   ith integer value
        ENDLOOP
      ELSE
        <INT>          FP_FULL
        <INT>          foff10
        <F4ARRAY>      array
      ENDIF

------------------------------------------------------------------------

    Input:
      fid       -  pointer to PFF file structure
      precision -  Floating-point precision type
      len       -  length of array written to PFF file
      farray    -  pointer to float array to be written to PFF file
      foff10    -  power-of-ten multiplier of "farray"
      ierr      -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   =  0,  No Error
                   =  1,  Error allocating temporary array

    Return Value: Number of floating point values written
*/
{
  static float    r216m1    = 1.525902189669642176e-5f;
  static int      maxsi2    =  32767;
  static int      minsi2    = -32768;
  static float    wgtmax    = 0.500007629511f;
  static float    wgtmin    = 0.499992370489f;
  static char    *module    = "PF_WR_FLTARRAY";
  register long   i, xsign, ival;
  int             tmpint;
  long            flen, len_all;
  long            message[2];
  int             prev_rank,  next_rank, last_rank;
  float           maxv, minv, offset, rscale, scale, fshift, tmpflt;
  int            *iptr = NULL;
  long           *proc_cnts = NULL;
  MPI_Status      tmpstat;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return 0L;

  if ( fid->mp_current == 0 && fid->mp_mode == PFFMP_CURRENT ) return 0L;

  if ( precision == FP_FULL )   {
    /* write out full-precision flag and base-10 offset */
    PFF_work_buf[0] = FP_FULL;  PFF_work_buf[1] = foff10;
    pf_u_sio( fid, WR, 2, PFF_work_buf, ierr );
    flen = 2*len;
    scale = 2.0f*FLT_MIN;
  }
  else   {
    flen = len;
    
    /* Search data to find array offset and scale info  */
    minv = FLT_MAX;
    maxv = -FLT_MAX;
    for ( i=0; i<len; ++i)  {
      minv = MIN( minv, farray[i] );
      maxv = MAX( maxv, farray[i] );
    }
    if ( fid->mp_mode == PFFMP_GLOBAL ) {
      PFFMP_Allreduce_1(fid, minv, tmpflt, MPI_FLOAT, MPI_MIN);
      PFFMP_Allreduce_1(fid, maxv, tmpflt, MPI_FLOAT, MPI_MAX);
    }
    
    /* Compute offset and scale factor */
    if ( minv == FLT_MAX || maxv == -FLT_MAX ) {
      offset = 0.0;
      scale = 1.0;
      rscale = 1.0;
    }
    else {
      offset = maxv*wgtmax + minv*wgtmin;
      scale = (maxv - minv)*r216m1;
      if ( scale > FLT_MIN )
        rscale = 1.0f/scale;
    }
    
    /* Encode and write out "offset" and "scale" */
    pf_u_f2i( offset, foff10, PFF_work_buf, ierr);
    pf_u_f2i( scale, foff10, PFF_work_buf+3, ierr);
    pf_u_sio( fid, WR, 6, PFF_work_buf, ierr );
  }

  /* Now find total array length (if global) */
  len_all = len;
  if ( fid->mp_mode == PFFMP_GLOBAL ) {
    if( (proc_cnts = (long *) malloc(PFFMP_procs*sizeof(long) )) == NULL ) {
      *ierr = 1;
    }
    PFFMP_Allreduce_1(fid, *ierr, tmpint, MPI_INT, MPI_MAX);
    if ( *ierr != 0 ) {
      pf_wr_err ( module, *ierr, fid, "Error allocating temporary array");
      return 0L;
    }
    PFFMP_Allgather_1(fid, len,proc_cnts,MPI_LONG);
    len_all = 0;
    for(i=0;i<PFFMP_procs;++i) len_all += proc_cnts[i];
  }

  /* Now write out length as 3 2-byte integers */
  pf_u_l2i( len_all, PFF_work_buf, ierr);
  pf_u_sio( fid, WR, 3, PFF_work_buf, ierr );

  if ( fid->mp_mode == PFFMP_GLOBAL ) {
    assert((PFFMP_rank==0 && fid->mp_current) || 
           (PFFMP_rank!=0 && fid->mp_current==0));
    prev_rank = -1;  /* which task writes before me */
    next_rank = -1;  /* which task writes after me */
    last_rank = -1;  /* last rank that writes data */
    if ( PFFMP_rank > 0 ) prev_rank = 0;

    for(i=0; i<PFFMP_procs; ++i) {
      if ( proc_cnts[i] > 0 ) {
        if ( i < PFFMP_rank ) {
          prev_rank = i;
        }
        else if ( i == PFFMP_rank ) {
          if ( prev_rank >= 0 ) {
            /* wait for prev task */
            PFFMP_Recv(message,2,prev_rank,MPI_LONG,0,tmpstat);
            *ierr = (int) message[1];
            pf_u_processor_toggle(fid,message[0],ierr);
          }
        }
        else if ( next_rank < 0 )  next_rank = i;
        last_rank = i;
      }
    }
  }

  if( len > 0 ) {
    /* try to get storage for tmp integer array */
    if ( (iptr = (int *) malloc( flen*sizeof(int) )) == NULL )  {
      *ierr = 1;
      pf_wr_err ( module, *ierr, fid, "Error allocating temporary array");
    }
    else      {
      /* Transfer floats to integer array; write out integer array;
      free temporary array */
      if ( flen == len )  {
        if ( scale > FLT_MIN )  {
          for ( i=0; i<len; ++i)   {
            if ( (fshift = farray[i] - offset) < 0 )  {
              fshift = -fshift;
              xsign = -1;
            }  else  xsign = 1;
            
            ival = xsign*( (int) (rscale*fshift + 0.5) );
            ival = MAX( minsi2, ival );
            iptr[i] = MIN( maxsi2, ival );
          }
        }
        else  for ( i=0; i<len; ++i) iptr[i] = 0;
        
        pf_u_sio( fid, WR, len, iptr, ierr );
      }
      else   {
        pf_u_f4 ( WR, len, iptr, farray, ierr );
        pf_u_sio( fid, WR, flen, iptr, ierr );
      }
      free( iptr );
    }
  }
  else /* if ( len == 0 ) */ {
    if ( PFFMP_rank != 0 ) return len_all;
  }
  
  if ( fid->mp_mode == PFFMP_GLOBAL ) {
    message[0] = fid->position;
    message[1] = *ierr;

    /* if I'm last task, need to send message to proc 0 */
    if ( next_rank < 0 && PFFMP_rank > 0 ) next_rank = 0;
    /* don't toggle the file off if Proc 0 is the only processor writing */
    if ( PFFMP_rank > 0 || last_rank > 0 ) pf_u_processor_toggle(fid,0L,ierr);

  
    /* I'm done -- send info to next task, if there is one */
    if ( next_rank >= 0 ) PFFMP_Send(message,2,next_rank,MPI_LONG,0);

    /* If I'm processor 0, and any other processor wrote something, I need to
       receive the message from the last processor and clean up */
    if ( PFFMP_rank == 0 && last_rank > 0 ) {
      PFFMP_Recv(message,2,last_rank,MPI_LONG,0,tmpstat);
      *ierr = (int) message[1];
      pf_u_processor_toggle(fid,message[0],ierr);
    }
    free(proc_cnts);
  }

  return len_all;
}
