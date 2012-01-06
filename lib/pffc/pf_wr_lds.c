/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_wr_lds.c
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

#include    "u_defs.h"

/*  Declare function */

#ifdef __STDC__

void pf_wr_lds ( PFFfid *fid, long adr1st, long *lds, int *ierr );

void pf_wr_lds ( PFFfid *fid, long adr1st, long *lds, int *ierr )

#else

void       pf_wr_lds  ();

void pf_wr_lds ( fid, adr1st, lds, ierr )

PFFfid   *fid; 
long      adr1st;
long     *lds;
int      *ierr;

#endif

/* 
       This routine is a WRITE routine that writes a dataset's length
       BACK into its header after the entire dataset has been
       written.
       This operation is ONLY ALLOWED in WRITE READ/WRITE mode !!!
       This routine should be call immediately AFTER all user data
       has been written to the dataset.  In addition, it must be
       supplied as input the file address of the dataset's header
       framing word.  NOTE that this address is returned in the
       variable "lstadr" from the call to pf_wr_header

    Input:
      fid     -  pointer to PFF file structure
      adr1st  -  file address of the dataset's header framing word (in
                 units of 16-bit words)
      ierr    -  If not zero, return with no operation

    Output:
      lds     -  pointer to dataset length (in 16-bit words)
      ierr    -  error flag:
                   =  0,  No Error
                   =  1,  GLOBAL mode not allowed for header
                  !=  0,  error encountered in called PFF routine
*/
{
  static char    *module    = "PF_WR_LDS";
  long            adrend, lenloc;
  int             buf[3];

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  if ( fid->mp_mode == PFFMP_GLOBAL ) {
    *ierr = 1;
    pf_wr_err ( module, *ierr, fid, "GLOBAL mode not allowed for header");
    return;
  }

  if ( fid->mp_current == 0 ) {
    *lds = 0;
    return;
  }

  /* Find current file position */
  adrend = pf_u_tell ( fid, ierr );
  if( *ierr != 0 ) return;

  /* compute address of dataset-length field and dataset length */
  lenloc = adr1st + 1;
  *lds   = adrend - adr1st;

  /* Encode dataset length to <LONG> */
  pf_u_l2i ( *lds, buf, ierr );
  if( *ierr != 0 ) return;

  /* Write length back to header location */
  pf_u_wrback (fid, lenloc, 3, buf, ierr);

  return;
}
