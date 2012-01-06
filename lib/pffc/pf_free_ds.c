/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_free_ds.c
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

/*  Declare function */

#ifdef __STDC__

void pf_free_ds ( PFFds_any *ds, int *ierr );

void pf_free_ds ( PFFds_any *ds, int *ierr )

#else

void       pf_free_ds    ();

void pf_free_ds ( ds, ierr )

PFFds_any      *ds; 
int            *ierr;

#endif

/* Release any PFF dataset structure

------------------------------------------------------------------------

    Input:
      ds      -  pointer to any PFF dataset structure to be released
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   =  0,  Normal return
                   =  1,  Unknown dataset type
*/
{
  static char    *module    = "PF_FREE_DS";

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  /* Delete the structure only if it is not null */

  if ( ds != NULL )   {
    if ( ds->type < 0  || ds->type > PFFMAXDS )  {
      *ierr = 1;
      pf_wr_err ( module, *ierr, NULL, "Unknown dataset type");
    }
    else    {
      (free_functs[ds->type]) ( ds, ierr );
    }
  }

  return;
}
