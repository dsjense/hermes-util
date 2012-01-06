/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_get_fp_precision.c
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

#include "filestack.h"
#include  "get_defs.h"
#include    "u_defs.h"

/*  Declare function */

#ifdef __STDC__

int pf_get_fp_precision ( PFFfid *fid,  int *ierr );

int pf_get_fp_precision ( PFFfid *fid,  int *ierr )

#else

int pf_get_fp_precision ();

int pf_get_fp_precision ( fid, ierr )

PFFfid  *fid;
int     *ierr;

#endif

/* Queries the precision for writing <FARRAY> primitives to the specified PFF
   file.  If the pointer to the pff file structure (fid) is NULL, PFF's global
   precision state is queried. This state determines the initial precision state
   of a file when it is opened. The default for this global state is FP_REDU.
   Return value will be one of the following legal values:
          FP_REDU - reduced precision. <FARRAY>'s are linearly mapped
                    to a 2-byte integer array.
          FP_FULL - full IEEE 32-bit precision

    Input:
      fid     -  pointer to PFF file structure (if NULL, PFF's global precision 
                 state is queried).
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   = 0,  Normal return
                   = 1,  Illegal File ID (FID)
   Return value:
       if ierr =  0;  current precision state is returned.
       if ierr != 0;  0 is returned.
*/
{
  static char        *module    = "PF_GET_FP_PRECISION";

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return 0;

  if ( (fid != NULL) && ( fid->stream == NULL ) ) {
    *ierr = 1;
    pf_wr_err ( module, *ierr, fid, "Illegal File ID (FID)" );
    return 0;
  }

  /* get the current value for return to calling function */
  if ( fid ) return fid->fp_precision;
  else       return PFF_fp_precision;
}
