/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_get_fid.c
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
#include "filestack.h"

/*  Declare function */

#ifdef __STDC__

PFFfid *pf_get_fid ( int entry, int *ierr );

PFFfid *pf_get_fid ( int entry, int *ierr )

#else

PFFfid      *pf_get_fid      ();

PFFfid *pf_get_fid ( entry, ierr )

int      entry;
int     *ierr;

#endif

/* This routine returns a PFF file element (FID) pointer assocated with a 
   specific entry number.

    Input:
      entry   -  # of file entry 
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   = 0,  Normal return
                   = 1,  Entry # is out of range
*/
{
  static char        *module    = "PF_GET_FID";
  PFFfid             *fid;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return NULL;

  if (  (fid = PFF.top)  ==  NULL ) return NULL;

  if ( ( entry < 1 )  ||  ( entry > fid->count ) )    {
    *ierr = 1;
    pf_wr_err ( module, *ierr, fid, "Entry # is out of range" );
    return NULL;
  }

  while ( fid != NULL && fid->count != entry )
    fid = fid->down;

  return fid;
}
