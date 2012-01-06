/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_bld_dir.c
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
#include "bld_defs.h"

/*  Declare function */

#ifdef __STDC__

PFFds_dir *pf_bld_dir ( PFFhead *head, long offset, int new_head, int *ierr );

PFFds_dir *pf_bld_dir ( PFFhead *head, long offset, int new_head, int *ierr )

#else

PFFds_dir   *pf_bld_dir    ();

PFFds_dir *pf_bld_dir ( head, offset, new_head, ierr )

PFFhead *head;
long offset;
int new_head;
int *ierr;

#endif

/* 
------------------------------------------------------------------------

       - This routine is a BLD routine that builds a Directory (DIR) 
         dataset from supplied data.
       - Dataset Format:
           <HEADER>       PFTDIR
           <INT>          TRAW         (raw type of referenced dataset)
           <LONG>         LENDIR       (length of referenced dataset)
           <LONG>         LOCDIR       (file offset of referenced dataset)

------------------------------------------------------------------------

    Input:
      head     -  pointer to PFF header structure
      offset   -  file offset of dataset (in 2-byte words)
      new_head -  flag indicating that a new header structure must be 
                  created because the supplied header must be preserved
                  WARNING: if FALSE, it is critical that "head" not be 
                  later freed by the calling program.  Immediately setting 
                  "head = NULL" will help prevent this.
      ierr     -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   =  0,  Normal return
                   =  2,  Error allocating memory for DIR structure
*/
{
  static char    *module    = "PF_BLD_DIR";
  PFFhead        *nhead =  NULL; 
  PFFds_dir      *dir   =  NULL;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return NULL;

  /* get memory for DIR structure */

  if ( (dir = (PFFds_dir *) malloc( sizeof(PFFds_dir) )) == NULL )  {
    *ierr = 2;
    pf_wr_err ( module, *ierr, NULL, 
                "Error allocating memory for DIR structure");
    return NULL;
  }
  else  {
    dir->type     = PFTDIR;
    dir->ref_type = head->rawtype;
    dir->length   = head->length;
    dir->offset   = offset;
  }

  if ( new_head )  {
    nhead = pf_bld_header ( PFTDIR, head->apptype, head->title,
                            head->type_name, head->length, 
                            head->ds_version, TRUE, ierr );
    if( *ierr != 0 )   {
      CHKFREE(dir);
      return NULL;
    }
    dir->head     = nhead;
  }
  else   {
    head->rawtype = PFTDIR;
    dir->head = head;
  }

  return dir;
}
