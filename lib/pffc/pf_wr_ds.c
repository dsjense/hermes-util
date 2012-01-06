/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_wr_ds.c
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
#include   "wr_defs.h"

/*  Declare function */

#ifdef __STDC__

void pf_wr_ds ( PFFfid   *fid, PFFds_any *ds, int *ierr );

void pf_wr_ds ( PFFfid   *fid, PFFds_any *ds, int *ierr )

#else

void pf_wr_ds ();

void pf_wr_ds ( fid, ds, ierr )

PFFfid      *fid; 
PFFds_any   *ds;
int         *ierr;

#endif

/* Read any PFF dataset structure

------------------------------------------------------------------------

    Input:
      fid     -  pointer to PFF file structure
      ds      -  general dataset structure to be written to file
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   =  0,  Normal return
                   = -2,  Unknown dataset type  (not really an error)
*/
{
  /* static char    *module    = "PF_WR_DS"; */

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  /* Write the dataset only if it is a known dataset type */

  if ( ds->type < 0  || ds->type > PFFMAXDS )  *ierr = -2;
  else    (write_functs[ds->type]) ( fid, ds, ierr );

  return;
}
