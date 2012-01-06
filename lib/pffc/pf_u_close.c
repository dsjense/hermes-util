/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_u_close.c
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
#include "ds_structs.h"
#include "filestack.h"

#include  "dir_defs.h"
#include "free_defs.h"
#include    "u_defs.h"
#include   "wr_defs.h"

/*  Declare function */

#ifdef __STDC__

void pf_u_close ( PFFfid *fid, int *ierr );

void pf_u_close ( PFFfid *fid, int *ierr )

#else

void       pf_u_close     ();

void pf_u_close ( fid, ierr)

PFFfid *fid; 
int *ierr;

#endif

/* Close a PFF I/O file and modifiy appropriate structures

    Input:
      fid     -  pointer to PFF file structure
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   = 0,  Normal return
                   = 1,  Illegal File ID (FID)
*/
{
  static char    *module    = "PF_U_CLOSE";
  register int    i;
  PFFfid         *tmpfid,*up,*down;
  PFFdir         *dir;
  PFFds_dir      *dsdir;
  int             l;
  long            ldptr;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  if ( fid->mp_current == 0 ) return;

  if ( fid == NULL )
    tmpfid = PFF.top;
  else
    tmpfid = fid;

  while ( tmpfid != NULL )  {

    if ( tmpfid->stream == NULL ) {
      *ierr = 1;
      pf_wr_err ( module, *ierr, fid, "Illegal File ID (FID)" );
      return;
    }

    down = tmpfid->down;
    up   = tmpfid->up;

    if ( tmpfid->extend_flag == TRUE ) {
      if ( tmpfid->mode == RW )
        pf_u_seek ( tmpfid, tmpfid->last_word, ierr);

      PFF_work_buf[0] = EOFFLG;
      pf_u_sio( tmpfid, WR, 1, PFF_work_buf, ierr );

      /* write pointer to directory info into file header */
      /*
            pf_u_l2i( tmpfid->position , iarr, ierr );
            pf_u_wrback ( tmpfid, 1, 3, iarr, ierr);
      */
      pf_wr_lds ( tmpfid, 0, &ldptr, ierr);

      /* Write directory to file now by looping over file's directory list */
      dir = tmpfid->dirbottom;
      while ( dir != NULL )  {
        /* get a directory dataset for this directory list entry */
        dsdir = pf_dir_get( dir, FALSE, ierr );
        /* write directory dataset to file; then free it */
        pf_wr_dir ( tmpfid, (PFFds_any *) dsdir, ierr );
        pf_free_dir ( (PFFds_any *) dsdir, ierr );
        dir = dir->up;
      }

      /* fill remainder of buffer (at least one word) with EOF flag */
      l = PFF_RECLEN - tmpfid->position % PFF_RECLEN;
      for( i=0; i < l; ++i)  {
        PFF_work_buf[i] = EOFFLG;
      }
      pf_u_sio( tmpfid, WR, l, PFF_work_buf, ierr );
      if( *ierr != 0 ) break;
    }

    i = fclose(tmpfid->stream);

    pf_free_dirlist(tmpfid->dirtop,ierr);   /*  recursively */

    if( up == NULL )
      PFF.top = down;
    else
      up->down = down;
    if( down != NULL )
      down->up = up;

    if ( tmpfid == PFF.current )   {
      if( up == NULL )
        PFF.current = down;
      else
        PFF.current = up;
    }

    for( i=tmpfid->count; up!=NULL; up=up->up)
      up->count = i++;

    CHKFREE (tmpfid->name);
    free(tmpfid);

    if ( fid != NULL ) break;

    tmpfid=down;
  }
}
