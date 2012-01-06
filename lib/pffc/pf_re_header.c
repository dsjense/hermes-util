/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_re_header.c
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
#include "ds_structs.h"
#include "workspace.h"

#include   "re_defs.h"
#include    "u_defs.h"

/*  Declare function */

#ifdef __STDC__

PFFhead *pf_re_header ( PFFfid *fid, long *offset, int *ierr );

PFFhead *pf_re_header ( PFFfid *fid, long *offset, int *ierr )

#else

PFFhead   *pf_re_header    ();

PFFhead *pf_re_header ( fid, offset, ierr )

PFFfid   *fid; 
long     *offset;
int      *ierr;

#endif

/* Read a dataset header from a PFF file: 

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
      ierr    -  If not zero, return with no operation

    Output:
      offset  -  file offset to first word of dataset (also FW of header)
                 (on error, it is set to the file offset at function entry)
      ierr    -  error flag:
                   =  0,  Normal return
                   = -1,  EOF reached  (not really an error)
                   =  1,  File Framing Error
                   =  2,  Error allocating header structure
*/
{
  static char    *module    = "PF_RE_HEADER";
  register int    i,j;
  long            nloc;
  int            *p;
  PFFhead        *head; 

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return NULL;

  *offset = pf_u_tell ( fid, ierr );

  pf_re_ds_frame ( fid, &nloc, ierr);
  if( *ierr != 0 ) return NULL;

  if ( (head = (PFFhead *) malloc( sizeof(PFFhead) )) == NULL )  {
    *ierr = 2;
    pf_wr_err ( module, *ierr, fid, "Error allocating header structure");
  }

  else    {

    pf_u_sio( fid, RE, 6, PFF_work_buf, ierr );
    pf_u_i2l( PFF_work_buf, &(head->length), ierr);
    head->rawtype    = PFF_work_buf[3];
    head->ds_version = PFF_work_buf[4];
    head->apptype    = PFF_work_buf[5];

    pf_u_sio( fid, RE, PFF_MAXRFU, PFF_work_buf, ierr );
    for (i=0;i<PFF_MAXRFU;i++)
      if ( PFF_work_buf[i] == DFAULT ) break;
    head->nwords_rfu = i;

    if ( i > 0 )   {
      if ( ( head->rfu = (int *) malloc(i*sizeof(int)) ) != NULL ) {
        for (p=head->rfu,j=0;j<i;j++,p++)
          *p = PFF_work_buf[j];
      }
    }
    else
      head->rfu = NULL;

    head->type_name = NULL;
    head->title = NULL;
    pf_u_string_io ( fid, RE, 0, &(head->type_name), ierr);
    pf_u_string_io ( fid, RE, 0, &(head->title), ierr);

    if ( *ierr == 0 ) 
      *offset = nloc;
    else   {
      CHKFREE (head->rfu);
      CHKFREE (head->type_name);
      CHKFREE (head->title);
      free(head);
      head = NULL;
    }
  }

  return head;
}
