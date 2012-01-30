/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_set_fp_precision.c
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

int pf_set_fp_precision ( PFFfid *fid,  int value, int *ierr );

int pf_set_fp_precision ( PFFfid *fid,  int value, int *ierr )

#else

int pf_set_fp_precision ();

int pf_set_fp_precision ( fid, value, ierr )

PFFfid  *fid;
int      value;
int     *ierr;

#endif

/* Sets the precision for writing <FARRAY> primitives to the specified PFF
   file.  If the pointer to the pff file structure (fid) is NULL, PFF's global
   precision state is set. This state determines the initial precision state of
   a file when it is opened. The default for this global state is FP_REDU.

    Input:
      fid     -  pointer to PFF file structure (if NULL, PFF's global precision 
                 state is changed).
      value   -  desired precision. Legal values are:
                   FP_REDU   - reduced precision. <FARRAY>'s are linearly mapped
                               to a 2-byte integer array.
                   FP_ALLFULL - all <FARRAY>s at full (4-byte, IEEE) precision
                   FP_ORDFULL - ordinate <FARRAY>s at full (4-byte)
                                precision, other data at reduced prec.
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   = 0,  Normal return
                   = 1,  Illegal File ID (FID)
                   = 3,  Illegal precision value 
*/
{
  static char        *module    = "PF_SET_FP_PRECISION";
  int                 old_value;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return 0;

  if ( (fid != NULL) && (fid->mp_current == TRUE) && (fid->stream == NULL) ) {
    *ierr = 1;
    pf_wr_err ( module, *ierr, fid, "Illegal File ID (FID)" );
    return 0;
  }

  /* get the last value for return to calling function */
  if ( fid ) old_value = fid->fp_precision;
  else       old_value = PFF_fp_precision;

  /* Is the requested precision a legal value ? */
  if ( value != FP_REDU && value != FP_ALLFULL && value != FP_ORDFULL )  {
    *ierr = 3;
    pf_wr_err ( module, *ierr, fid, "Illegal precision value" );
    return old_value;
  }
  /* set global or file-specific precision state as specified */
  if ( fid ) fid->fp_precision = value;
  else       PFF_fp_precision = value;

  return old_value;
}
