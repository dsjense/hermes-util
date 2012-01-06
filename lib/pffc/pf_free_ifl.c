/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_free_ifl.c
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

/*  Declare function */

#ifdef __STDC__

void pf_free_ifl ( PFFds_any *ifl, int *ierr );

void pf_free_ifl ( PFFds_any *ifl, int *ierr )

#else

void       pf_free_ifl    ();

void pf_free_ifl ( ifl, ierr )

PFFds_any      *ifl; 
int            *ierr;

#endif

/* Release a PFF IFL dataset structure

------------------------------------------------------------------------

   Memory associated with internal pointers must also be freed:

       ifl->head
       ifl->iarr
       ifl->farr
       ifl->flist
       ifl->floff10

------------------------------------------------------------------------

    Input:
      ifl     -  pointer to IFL dataset structure to be released
      ierr    -  If not zero, return with no operation

    Output:
      NONE
*/
{
  /* static char    *module    = "PF_FREE_IFL"; */
  PFFds_ifl      *tmpifl;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  /* need a temporary pointer to IFL structure */
  tmpifl = ( PFFds_ifl* ) ifl;

  /* Delete the structure only if it is not null */

  if ( tmpifl != NULL )   {
    pf_free_header ( tmpifl->head, ierr );
    CHKFREE (tmpifl->iarr);
    CHKFREE (tmpifl->farr);
    CHKFREE (tmpifl->flist);
    CHKFREE (tmpifl->floff10);
    free(tmpifl);
    ifl = NULL;
  }

  return;
}
