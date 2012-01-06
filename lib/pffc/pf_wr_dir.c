/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_wr_dir.c
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
#include   "wr_defs.h"
#include    "u_defs.h"

/*  Declare function */

#ifdef __STDC__

void pf_wr_dir ( PFFfid   *fid ,  PFFds_any *ds, int *ierr );

void pf_wr_dir ( PFFfid   *fid ,  PFFds_any *ds, int *ierr )

#else

void pf_wr_dir ();
void pf_wr_dir ( fid, ds, ierr )

PFFfid   *fid; 
PFFds_any *ds;
int      *ierr;

#endif

/* 
------------------------------------------------------------------------

       - This routine is a WRITE routine that writes a Directory (DIR) 
         dataset to a PFF file.
       - This operation is ONLY ALLOWED in WRITE or Read/Write modes !!!
       - Dataset Format:
           <HEADER>       PFTDIR
           <INT>          TRAW         (raw type of referenced dataset)
           <LONG>         LENDIR       (length of referenced dataset)
           <LONG>         LOCDIR       (file offset of referenced dataset)

------------------------------------------------------------------------

    Input:
      fid     -  pointer to PFF file structure
      ds      -  general dataset structure to be written to file [needs to
                 be cast to (PFFds_dir *) before using]
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   =  0,        Normal return
                   =  1,        Incorrect dataset type
                   otherwise,   Error from called PFF utility routine
*/
{
  static char    *module    = "PF_WR_DIR";
  long            offset, lds;
  PFFds_dir      *dir;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  if ( ds->type != PFTDIR || (ds->head)->rawtype != PFTDIR )  {
    *ierr = 1;
    pf_wr_err ( module, *ierr, fid, "Incorrect dataset type");
  }

  /* Write header */
  pf_wr_header ( fid, ds->head, &offset, ierr );
  if( *ierr != 0 ) return;

  dir = (PFFds_dir *) ds;

  /* Write TRAW, LENDIR, & LOCDIR */

  PFF_work_buf[0] = dir->ref_type;
  pf_u_l2i( dir->length, PFF_work_buf+1, ierr);
  pf_u_l2i( dir->offset, PFF_work_buf+4, ierr);
  pf_u_sio( fid, WR, 7, PFF_work_buf, ierr );

  /* write the dataset length (of directory dataset, NOT referenced
     dataset) back to the header */
  pf_wr_lds ( fid, offset, &lds, ierr );

  /* DON'T PUT DIRECTORY INFO INTO MEMORY-RESIDENT DIRECTORY STRUCTURE
     SINCE WE ARE PROCESSING A DIRECTORY DATASET */

  return;
}
