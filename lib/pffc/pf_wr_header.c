/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_wr_header.c
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
#include "pffmp.h"
#include "ds_structs.h"
#include "workspace.h"

#include    "u_defs.h"

/*  Declare function */

#ifdef __STDC__

void pf_wr_header ( PFFfid *fid, PFFhead *head, long *lstadr, int *ierr );

void pf_wr_header ( PFFfid *fid, PFFhead *head, long *lstadr, int *ierr )

#else

void pf_wr_header ();

void pf_wr_header ( fid, head, lstadr, ierr )

PFFfid   *fid; 
PFFhead  *head;
long     *lstadr;
int      *ierr;

#endif

/* Write a dataset header to a PFF file: 

------------------------------------------------------------------------

    Dataset Header Format:
      <INT>          DFRAME       dataset framing word
      <LONG>         Lds          dataset length (in 2-byte words)
      <INT>          Traw         raw dataset type
      <INT>          Vds          raw dataset type version #
      <INT>          Tapp         application dataset type
      10x<INT>       RFU          reserved for future use
      <STRING>       TYPE         application dataset type label
      <STRING>       TITLE        dataset title/comment

------------------------------------------------------------------------

    Input:
      fid     -  pointer to PFF file structure
      head    -  header structure to be written to file
      ierr    -  If not zero, return with no operation

    Output:
      lstadr  -  file position pointer on subroutine entry
      ierr    -  error flag:
                   =  0,  Normal return
                   =  1,  Error allocating RFU array
                   =  2,  GLOBAL mode not allowed for header
                  !=  0,  error encountered
*/
{
  static char    *module    = "PF_WR_HEADER";
  register int    i;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  if ( fid->mp_mode == PFFMP_GLOBAL ) {
    *ierr = 2;
    pf_wr_err ( module, *ierr, fid, "GLOBAL mode not allowed for header");
    return;
  }

  if ( fid->mp_current == 0 ) {
    *lstadr = -1;
    return;
  }

  /* Find entry (current) file position */
  *lstadr = pf_u_tell ( fid, ierr );

  /* Write framing word */
  PFF_work_buf[0] = DFRAME;
  pf_u_sio( fid, WR, 1, PFF_work_buf, ierr );
  if( *ierr != 0 ) return;

  if ( head->length > 0 )  {
    /* Encode dataset length to <LONG> if it's greater than zero */
    pf_u_l2i ( head->length, PFF_work_buf, ierr );
    if( *ierr != 0 ) return;
  }
  else  {
    /* otherwise, write DFAULT in encoded integers */
    PFF_work_buf[0] = DFAULT;
    PFF_work_buf[1] = DFAULT;
    PFF_work_buf[2] = DFAULT;
  }

  /* Write encoded length to file */
  pf_u_sio (fid, WR, 3, PFF_work_buf, ierr);
  if( *ierr != 0 ) return;

  /* Write out the raw dataset type, version #, and app. dataset type */
  PFF_work_buf[0] = head->rawtype;
  PFF_work_buf[1] = head->ds_version;
  PFF_work_buf[2] = head->apptype;
  pf_u_sio (fid, WR, 3, PFF_work_buf, ierr);
  if( *ierr != 0 ) return;

  /* check to see if dataset is being written w/ FP_FULL */
  if ( head->nwords_rfu == 0 )   {
    if ( fid->fp_precision == FP_FULL )   {
      if ( (head->rfu = (int *) malloc(sizeof(int))) == NULL )  {
        *ierr = 1;
        pf_wr_err ( module, *ierr, fid, "Error allocating RFU array");
      }
      else   {
        head->nwords_rfu = 1;
        head->rfu[0] = 0;
      }
    }
  }
  if ( head->nwords_rfu > 0 )   {
    if ( fid->fp_precision == FP_FULL )  head->rfu[0] |=  1;
    else                                 head->rfu[0] &= ~1;
  }

  /* Write out the Reserved words (max of PFF_MAXRFU); pad with DFAULT */
  for (i=0;i<head->nwords_rfu;i++) PFF_work_buf[i] = head->rfu[i];
  for (   ;i<PFF_MAXRFU;      i++) PFF_work_buf[i] = DFAULT;
  pf_u_sio( fid, WR, PFF_MAXRFU, PFF_work_buf, ierr );
  if( *ierr != 0 ) return;

  pf_u_string_io ( fid, WR, 0, &(head->type_name), ierr);
  pf_u_string_io ( fid, WR, 0, &(head->title), ierr);

  return;
}
