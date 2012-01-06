/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_re_dir.c
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

PFFds_any *pf_re_dir ( PFFfid   *fid ,int keep, int *ierr );

PFFds_any *pf_re_dir ( PFFfid   *fid ,int keep, int *ierr )

#else

PFFds_any   *pf_re_dir    ();

PFFds_any *pf_re_dir ( fid, keep, ierr )

PFFfid   *fid; 
int       keep;
int      *ierr;

#endif

/* 
------------------------------------------------------------------------

       - This routine is a READ routine that reads a Directory (DIR) 
         dataset from a PFF file.
       - This operation is ONLY ALLOWED in READ mode !!!
       - Dataset Format:
           <HEADER>       PFTDIR
           <INT>          TRAW         (raw type of referenced dataset)
           <LONG>         LENDIR       (length of referenced dataset)
           <LONG>         LOCDIR       (file offset of referenced dataset)

------------------------------------------------------------------------

    Input:
      fid     -  pointer to PFF file structure
      keep    -  flag indicating whether or not to keep a non-zero value 
                 for floating data in the case of underflow
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   =  0,  Normal return
                   = -1,  EOF reached  (not really an error)
                   =  1,  Incorrect dataset type
                   =  2,  Error allocating memory for DIR structure
*/
{
  static char    *module    = "PF_RE_DIR";
  long            offset;
  PFFhead        *head =  NULL; 
  PFFds_dir      *dir  =  NULL;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return NULL;

  head = pf_re_header ( fid, &offset, ierr );
  if( *ierr != 0 ) return NULL;

  if ( head->rawtype != PFTDIR )  {
    *ierr = 1;
    pf_wr_err ( module, *ierr, fid, "Incorrect dataset type");
  }

  else    {

    /* get memory for DIR structure */

    if ( (dir = (PFFds_dir *) malloc( sizeof(PFFds_dir) )) == NULL )  {
      *ierr = 2;
      pf_wr_err ( module, *ierr, fid, 
                  "Error allocating memory for DIR structure");
    }
    else  {

      dir->type     = PFTDIR;
      dir->head     = head;
      head = NULL;

      /* read in information about referenced dataset */

      pf_u_sio( fid, RE, 7, PFF_work_buf, ierr );
      dir->ref_type = PFF_work_buf[0];
      pf_u_i2l( PFF_work_buf+1, &(dir->length), ierr);
      pf_u_i2l( PFF_work_buf+4, &(dir->offset), ierr);

    }
  }

  CHKFREE (head);
  return ( PFFds_any* ) dir;
}
