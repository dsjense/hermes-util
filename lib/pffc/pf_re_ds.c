/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_re_ds.c
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
#include "free_defs.h"
#include   "re_defs.h"
#include    "u_defs.h"

/*  Declare function */

#ifdef __STDC__

PFFds_any *pf_re_ds ( PFFfid   *fid ,int keep, int *ierr );

PFFds_any *pf_re_ds ( PFFfid   *fid ,int keep, int *ierr )

#else

PFFds_any *pf_re_ds ();

PFFds_any *pf_re_ds ( fid, keep, ierr )

PFFfid      *fid; 
int          keep;
int         *ierr;

#endif

/* Read any PFF dataset structure

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
                   = -2,  Unknown dataset type  (not really an error)

    Return Value:
      Pointer to dataset structure; for any error OTHER than (-2), NULL
      is returned.
*/
{
  /* static char    *module    = "PF_RE_DS"; */
  long            offset;
  int             rawtype;
  PFFhead        *head =  NULL; 
  PFFds_any      *ds   =  NULL;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return NULL;

  /* Read the dataset only if it is a known dataset type */

  head = pf_re_header ( fid, &offset, ierr );
  if( *ierr != 0 ) return NULL;

  pf_u_seek(fid,offset,ierr);

  rawtype = head->rawtype;
  if ( rawtype < 0  || rawtype > PFFMAXDS )  {
    /*     "Unknown dataset type"      */
    *ierr = -2;
    if ( ( ds = (PFFds_any *) malloc( sizeof(PFFds_any) )) != NULL )  {
      ds->type = rawtype;
      ds->head = head;
    }
  }
  else    {
    pf_free_header ( head, ierr);
    ds = (PFFds_any *) (read_functs[rawtype]) ( fid, keep, ierr );
  }

  return ds;
}
